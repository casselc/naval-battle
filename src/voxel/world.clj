(ns voxel.world
  "Pure naval battle state, per the plan: two voxel warships duel on the
  particle ocean until one sinks.

  The fleet is plain data. Each ship is a cell map posed with quaternion +
  anchor (the voxel.buoyancy convention); voxel.physics owns the real Box3D
  motion and reports plain-data facts that step-state folds back in. Shells
  fly pure ballistics here — no physics body per shell — substepped so a
  30 m/s round cannot tunnel a 1 m hull. A hit carves every cell within the
  blast radius of the impact point; the exposed faces flood through
  voxel.physics, the wreck settles lower, and a ship whose origin passes
  SUNK-DEPTH under the waterline is sunk. The battle is over when one fleet
  remains afloat.

  The enemy is an AI that leads the player and fires on cooldown whenever a
  ballistic solution exists. The ocean (voxel.ocean) steps in lockstep,
  feeling every hull (displacement push + wake vorticity) and every blast
  and splash (spray + swirl), so the sea is a first-class combatant rather
  than scenery."
  (:require [voxel.buoyancy :as buoy]
            [voxel.camera :as cam]
            [voxel.ship :as ship]
            [voxel.ocean :as sea]))

(def GRAVITY 25.0)
(def BALL-RADIUS 0.5)
(def SHELL-SPEED 30.0)
(def FIRE-COOLDOWN 3.0)
(def BLAST-RADIUS 2.6)
(def SUNK-DEPTH 6.0)
(def SHELL-LIFETIME 12.0)
;; 56 units apart: half again beyond the 36-unit low-arc gun range
;; (v^2/g), so the fleets must properly sail in before the guns speak
(def PLAYER-POS [0.0 -3.0 -28.0])
(def ENEMY-POS [0.0 -3.0 28.0])

;; --- fleet -----------------------------------------------------------------

(defn make-ship
  "A fleet record from a voxel.ship layout, posed at pos with the given
  yaw (bow at +k in layout space)."
  [id layout pos yaw]
  (let [cells (:cells layout)]
    {:id id
     :ai false
     :cells cells
     :anchor (:anchor layout)
     :guns (:guns layout)
     :skin (buoy/skin-faces cells)
     :faces (buoy/surface-faces cells)
     :pos pos
     :quat (buoy/yaw-quat yaw)
     :body nil
     :vel [0.0 0.0 0.0]
     :speed 0.0
     :cooldown 0.0
     :sunk false}))

;; The sheet is sized from the camera, not from the arena: it has to run
;; past every corner of the frame or the player sees the water end. Change
;; the vantage and voxel.camera moves this with it.
(def SEA-EXTENT (cam/sea-extent))
(def SEA-TARGET-SPACING 2.0)   ; one particle per tile, roughly this wide
(def SEA-COLS
  (int (inc (Math/round (/ (* 2.0 SEA-EXTENT) SEA-TARGET-SPACING)))))
(def SEA-SPACING (/ (* 2.0 SEA-EXTENT) (dec (double SEA-COLS))))
;; a few units of open water past the sheet so particles can drift before
;; the domain wall reflects them
(def SEA-BOUNDS (+ SEA-EXTENT 4.0))

(defn initial-state
  []
  {:phase :playing
   :ships {:player (make-ship :player (ship/dreadnought) PLAYER-POS 0.0)
           :enemy (assoc (make-ship :enemy (ship/dreadnought) ENEMY-POS Math/PI)
                         :ai true)}
   :shells []
   :ocean (sea/make-sea SEA-COLS SEA-EXTENT SEA-BOUNDS)
   :time 0.0
   :events []})

;; --- gunnery ------------------------------------------------------------------

(defn- dist-sq
  [a b]
  (reduce + (map #(* % %) (map - a b))))

(defn- muzzle-point
  "World point of a gun cell's muzzle: the cell centre, a cell and a half up
  so shells clear the turret."
  [ship [i j k]]
  (buoy/body-point->world ship [(+ i 0.5) (+ j 1.5) (+ k 0.5)]))

(defn- nearest-gun
  [ship target]
  (reduce (fn [a b]
            (if (< (dist-sq (muzzle-point ship b) target)
                   (dist-sq (muzzle-point ship a) target))
              b a))
          (:guns ship)))

(defn- aim-velocity
  "Muzzle velocity that lands a shell at target from p0 at the given speed:
  the low-arc ballistic solution, nil when the target is out of range."
  [p0 p1 speed]
  (let [dx (- (p1 0) (p0 0)) dy (- (p1 1) (p0 1)) dz (- (p1 2) (p0 2))
        d (Math/sqrt (+ (* dx dx) (* dz dz)))
        s2 (* speed speed)
        disc (- (* s2 s2) (* GRAVITY (+ (* GRAVITY d d) (* 2.0 dy s2))))]
    (when (and (> d 1e-6) (>= disc 0.0))
      (let [tan (/ (- s2 (Math/sqrt disc)) (* GRAVITY d))
            cos (/ 1.0 (Math/sqrt (+ 1.0 (* tan tan))))
            sin (* tan cos)]
        [(* speed cos (/ dx d)) (* speed sin) (* speed cos (/ dz d))]))))

(defn fire
  "Fire the ship's nearest gun at target [x y z] on the low ballistic arc
  for SHELL-SPEED. The state is returned unchanged when the battle is over,
  the ship is sunk or cooling down, or no ballistic solution exists."
  [state id target]
  (let [s (get-in state [:ships id])]
    (if (or (not= :playing (:phase state))
            (:sunk s)
            (pos? (or (:cooldown s) 0.0)))
      state
      (let [m (muzzle-point s (nearest-gun s target))
            v (aim-velocity m target SHELL-SPEED)]
        (if (nil? v)
          state
          (-> state
              (update :shells conj {:owner id :pos m :vel v :t 0.0})
              (assoc-in [:ships id :cooldown] FIRE-COOLDOWN)
              (update :events conj {:type :fired :ship id})))))))

(defn preview-arc
  "Closed-form sample of the shot `fire` would take at target right now:
  points from the muzzle to the first sea crossing, nil when the guns are
  cooling or no ballistic solution exists. The aim reticle draws these."
  [state id target]
  (let [s (get-in state [:ships id])]
    (when (and (= :playing (:phase state))
               (not (:sunk s))
               (not (pos? (or (:cooldown s) 0.0))))
      (let [m (muzzle-point s (nearest-gun s target))
            v (aim-velocity m target SHELL-SPEED)]
        (when v
          (loop [t 0.0 pts []]
            (let [p [(+ (m 0) (* (v 0) t))
                     (+ (m 1) (* (v 1) t) (* -0.5 GRAVITY t t))
                     (+ (m 2) (* (v 2) t))]]
              (if (or (<= (p 1) 0.0) (> t 8.0))
                (conj pts p)
                (recur (+ t 0.05) (conj pts p))))))))))

;; --- shell flight ----------------------------------------------------------------

(defn- world->local
  "World point into the ship's grid coordinates — the inverse of
  voxel.buoyancy/body-point->world."
  [{:keys [pos quat anchor]} p]
  (let [[qx qy qz qw] quat
        d (mapv - p pos)
        [rx ry rz] (buoy/q-rotate [(- qx) (- qy) (- qz) qw] d)]
    (mapv + anchor [rx ry rz])))

(defn- cell-at
  "The hull cell containing world point p, if any."
  [ship p]
  (let [v (world->local ship p)]
    [(int (Math/floor (v 0))) (int (Math/floor (v 1))) (int (Math/floor (v 2)))]))

(defn- carve-ship
  "The ship minus every cell whose world centre lies within radius of world
  point p, with the doomed cells."
  [ship p radius]
  (let [r2 (* radius radius)
        doomed (vec (for [[cell _] (:cells ship)
                          :let [c (buoy/cell-center->world ship cell)]
                          :when (<= (dist-sq c p) r2)]
                      cell))]
    (let [carved (update ship :cells #(apply dissoc % doomed))]
      [(assoc carved :faces (buoy/surface-faces (:cells carved))) doomed])))

(defn- shell-step
  "Advance one shell one frame, substepped so a fast shell cannot tunnel a
  1-cell hull. Returns [shell impacts]; the shell is nil once it is spent
  (hit, splash, expired or way out of the arena)."
  [ships shell dt]
  (let [v (:vel shell)
        speed (Math/sqrt (reduce + (map #(* % %) v)))
        n (max 1 (long (Math/ceil (* speed dt 3.0))))
        h (/ dt n)]
    (loop [i 0 sh shell]
      (if (= i n)
        [sh []]
        (let [sh (-> sh
                     (update :pos (fn [p] (mapv + p (map #(* % h) (:vel sh)))))
                     (update-in [:vel 1] - (* GRAVITY h))
                     (update :t + h))
              p (:pos sh)
              struck (first (keep (fn [[id s]]
                                    (when (and (not= id (:owner sh))
                                               (not (:sunk s))
                                               (contains? (:cells s) (cell-at s p)))
                                      id))
                                  ships))]
          (cond
            struck [nil [{:kind :hit :ship struck :point p}]]
            (< (p 1) 0.0) [nil [{:kind :splash :point p}]]
            (or (> (:t sh) SHELL-LIFETIME)
                (> (Math/abs (p 0)) 80.0) (> (Math/abs (p 2)) 80.0))
            [nil []]
            :else (recur (inc i) sh)))))))

(defn- apply-impact
  "Fold one shell impact into the state: carve the hull (or splash the sea)
  and feed the ocean a blast for the coupling."
  [state {:keys [kind ship point]}]
  (if (= :splash kind)
    (-> state
        (update :events conj {:type :splash :point point})
        (update :blasts conj {:x (point 0) :z (point 2) :r 2.5 :power 4.0}))
    (let [[carved doomed] (carve-ship (get-in state [:ships ship]) point BLAST-RADIUS)]
      (-> state
          (assoc-in [:ships ship] carved)
          (update :events conj {:type :blast :ship ship :point point
                                :destroyed (count doomed) :cells doomed})
          (update :blasts conj {:x (point 0) :z (point 2) :r 3.5 :power 10.0})))))

;; --- the enemy gunner --------------------------------------------------------------

(def ENGAGE-RANGE 30.0)  ; the AI sails in until the guns can reach

(defn- bow-dir
  "Unit bow direction of a ship in world space (bow at +k in layout)."
  [s]
  (buoy/q-rotate (:quat s) [0.0 0.0 1.0]))

(defn ai-helm
  "Helm order for an AI ship while the fleets close: full thrust and a
  hard turn toward the nearest live foe until inside ENGAGE-RANGE, then
  hold station and let the guns work. [thrust turn], right-positive."
  [state id]
  (let [s (get-in state [:ships id])
        foe (first (remove #(or (:sunk %) (= id (:id %))) (vals (:ships state))))]
    (if (or (nil? foe) (:sunk s) (not (:ai s)))
      [0.0 0.0]
      (let [[fx _ fz] (:pos foe)
            [sx _ sz] (:pos s)
            d (Math/sqrt (+ (* (- fx sx) (- fx sx)) (* (- fz sz) (- fz sz))))]
        (if (> d ENGAGE-RANGE)
          (let [[bx _ bz] (bow-dir s)
                cross-y (- (* bz (- fx sx)) (* bx (- fz sz)))]
            (if (< (Math/abs cross-y) 0.5)
              [1.0 0.0]
              [1.0 (Math/signum cross-y)]))
          [0.0 0.0])))))

(defn- aim-point
  "Where an AI gunner aims: the foe's position one estimated flight-time
  from now, raised to hull height."
  [shooter foe]
  (let [[tx ty tz] (:pos foe)
        [vx _ vz] (or (:vel foe) [0.0 0.0 0.0])
        [sx _ sz] (:pos shooter)
        d (Math/sqrt (+ (* (- tx sx) (- tx sx)) (* (- tz sz) (- tz sz))))
        t (/ d SHELL-SPEED)]
    [(+ tx (* vx t)) (+ ty 1.0) (+ tz (* vz t))]))

(defn- ai-engage
  [st id]
  (let [foe (first (remove :sunk (map val (dissoc (:ships st) id))))]
    (if (nil? foe)
      st
      (fire st id (aim-point (get-in st [:ships id]) foe)))))

(defn- ai-turn
  [st]
  (reduce (fn [st [id s]] (if (:ai s) (ai-engage st id) st)) st (:ships st)))

;; --- stepping ---------------------------------------------------------------------

(defn- fold-facts
  "Fold one frame of voxel.physics body facts into the fleet, marking ships
  whose origin has passed SUNK-DEPTH below the waterline as sunk."
  [ships facts]
  (let [by-body (into {} (map (juxt :body identity) (:bodies facts)))]
    (into {} (map (fn [entry]
                    (let [id (key entry)
                          s (val entry)]
                      [id (if-let [f (and (:body s) (get by-body (:body s)))]
                            (let [moved (assoc s :pos (:pos f) :quat (:quat f)
                                               :vel (or (:vel f) [0.0 0.0 0.0])
                                               :speed (or (:speed f) 0.0))]
                              (if (and (not (:sunk moved))
                                       (< (second (:pos f)) (- SUNK-DEPTH)))
                                (assoc moved :sunk true)
                                moved))
                            s)]))
                  ships))))

(defn- hulls-of
  "The coupling each floating hull feeds the ocean: displacement push and
  wake vorticity scaled by speed (a becalmed ship leaves still water)."
  [st]
  (for [[_ s] (:ships st) :when (not (:sunk s))]
    (let [v (or (:speed s) 0.0)]
      {:x ((:pos s) 0) :z ((:pos s) 2) :r 5.0
       :push (* 0.6 v) :swirl (* 0.3 v)})))

(defn- decide
  "The battle is over once exactly one fleet is still afloat."
  [st]
  (if (not= :playing (:phase st))
    st
    (let [afloat (remove :sunk (vals (:ships st)))]
      (if (= 1 (count afloat))
        (assoc st :phase :over :winner (:id (first afloat)))
        st))))

(defn step-state
  "Advance the battle dt seconds. facts is voxel.physics/step! output,
  folded into the fleet first. Events accumulate (capped) for the HUD."
  [state dt facts]
  (let [ships (fold-facts (:ships state) facts)
        sunk-now (keep (fn [[id s]]
                         (when (and (:sunk s) (not (:sunk (get-in state [:ships id]))))
                           {:type :sunk :ship id}))
                       ships)
        st (-> state
               (assoc :ships ships)
               (update :events into sunk-now)
               (assoc :blasts []))
        [shells impacts] (reduce (fn [[ss imps] sh]
                                   (let [[sh' imps'] (shell-step ships sh dt)]
                                     [(if sh' (conj ss sh') ss) (into imps imps')]))
                                 [[] []] (:shells st))
        st (reduce apply-impact (assoc st :shells shells) impacts)
        st (ai-turn st)
        st (update st :ships (fn [ss]
                               (into {} (map (fn [[id s]]
                                               [id (update s :cooldown #(max 0.0 (- % dt)))])
                                             ss))))
        ocean (sea/step-ocean (:ocean st) dt (:blasts st) (hulls-of st))]
    (-> st
        (assoc :ocean ocean)
        (assoc :time (+ (or (:time st) 0.0) dt))
        (update :events (fn [es] (vec (take-last 200 es))))
        (dissoc :blasts)
        decide)))

;; --- physics wiring ---------------------------------------------------------------

(defn attach-bodies
  "Attach Box3D body ids to the fleet after voxel.physics spawned them."
  [state id->body]
  (update state :ships
          (fn [ss]
            (into {} (map (fn [[id s]]
                            [id (if-let [b (get id->body id)]
                                  (assoc s :body b)
                                  s)])
                          ss)))))

(defn live-body-ids
  "Every Box3D body id still referenced by the world. voxel.physics
  destroys the rest."
  [state]
  (into #{} (keep :body (vals (:ships state)))))
