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

;; Shells fly here, not in Box3D, so they get their own gravity - and that
;; is the knob that decides whether a ship can be missed. Gun range is
;; v^2/g and time of flight is about d/v, so holding v^2/g fixed while
;; lowering both keeps the range and the shape of the arc exactly as they
;; were and just gives the shell longer in the air. At 30 m/s a round
;; crossed the arena in 1.2s, which is less time than a dreadnought needs to
;; move her own beam - every shot with a decent lead connected, and evading
;; was pointless. These give 2.1s at fighting range for the same 36-unit
;; reach.
(def SHELL-SPEED 16.0)
(def SHELL-GRAVITY 6.0)
(def GUN-RANGE (/ (* SHELL-SPEED SHELL-SPEED) SHELL-GRAVITY))

(def FIRE-COOLDOWN 3.0)
(def BLAST-RADIUS 2.6)
(def SUNK-DEPTH 6.0)
(def SHELL-LIFETIME 20.0)
;; a shell past this has left the battle; the fleets spawn at 62 and the
;; simulated sea runs to 93, so it has to be wider than either
(def ARENA-LIMIT 120.0)
;; well over three times gun range apart: the fleets have a proper approach
;; to sail before anyone is in a position to shoot
(def PLAYER-POS [0.0 -3.0 -62.0])
(def ENEMY-POS [0.0 -3.0 62.0])

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

(defn flat-range
  "Horizontal distance between two world points - the range a gun lays for."
  [a b]
  (let [dx (- (b 0) (a 0)) dz (- (b 2) (a 2))]
    (Math/sqrt (+ (* dx dx) (* dz dz)))))

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
        disc (- (* s2 s2)
                (* SHELL-GRAVITY (+ (* SHELL-GRAVITY d d) (* 2.0 dy s2))))]
    (when (and (> d 1e-6) (>= disc 0.0))
      (let [tan (/ (- s2 (Math/sqrt disc)) (* SHELL-GRAVITY d))
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
                     (+ (m 1) (* (v 1) t) (* -0.5 SHELL-GRAVITY t t))
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
                     (update-in [:vel 1] - (* SHELL-GRAVITY h))
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
                (> (Math/abs (p 0)) ARENA-LIMIT)
                (> (Math/abs (p 2)) ARENA-LIMIT))
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

;; Where she wants the fight. Time of flight is the only thing that lets a
;; 26-unit hull be somewhere else when the shell lands, and it grows with
;; range: measured, a hard turn takes her 4.4 units off her track in a
;; shell's flight at range 34 and only 1.3 at range 24. So the edge of her
;; own reach is the safe place to fight and the middle of it is not - which
;; is why she works to hold a standoff instead of closing to point blank.
(def STANDOFF (* 0.85 GUN-RANGE))
;; Inside this she breaks off. Two things bite, and the hulls bite first: a
;; shell starts arriving faster than she can be elsewhere, and - since these
;; are 26-unit ships - a range measured centre to centre that is barely more
;; than a ship's length is already an overlap when they are bow on. Ramming
;; put a hull under with its armour untouched.
(def KNIFE-RANGE (max (* 0.55 GUN-RANGE) (* 1.25 ship/LENGTH)))
;; a gun that comes up within a shell's flight is a gun to worry about now
(def ^:private THREAT-HORIZON 2.4)
;; How hard she leans on and off the bearing to correct the range. She turns
;; at about 19 deg/s, so swinging from a head-on approach onto her orbit
;; takes some four seconds, in which the two of them close another thirty
;; units - the flare has to start that far outside the standoff or she blows
;; straight through it into a brawl. A tenth of a unit per unit of error did
;; exactly that.
(def ^:private RANGE-GAIN 0.06)
;; ...and how hard she leans against the rate she is closing at. Range error
;; alone is proportional control through a four-second lag, which oscillates:
;; she overshot the standoff, ran out past it, came back in, and wandered
;; between 20 and 65 units for the whole battle.
(def ^:private RANGE-DAMP 0.15)
(def ^:private RANGE-LEAN-MAX 2.5)
(def ^:private HELM-DEADBAND 0.10)
;; How far either side of her base course she snakes while a gun is on her.
;; Reversing which way round the foe she goes would be a bigger dodge, but a
;; 180-degree turn takes nine seconds at her rate of turn and his gun cycles
;; in three, so she would never finish one - the helm stays saturated, she
;; never actually orbits, and the range runs away. Weaving either side of the
;; course keeps her turning without giving up station-keeping.
(def ^:private WEAVE-ANGLE 0.6)

(defn- bow-dir
  "Unit bow direction of a ship in world space (bow at +k in layout)."
  [s]
  (buoy/q-rotate (:quat s) [0.0 0.0 1.0]))

(defn gun-threat
  "How long until the foe could put a shell in the air at s: their remaining
  reload, or nil when their guns cannot reach her at all. This is the thing
  the AI steers by - not the foe's position, but the state of his guns."
  [s foe]
  (when (and foe
             (not (:sunk foe))
             (<= (flat-range (:pos s) (:pos foe)) GUN-RANGE))
    (max 0.0 (double (or (:cooldown foe) 0.0)))))

(defn- live-foe
  [state id]
  (first (remove #(or (:sunk %) (= id (:id %))) (vals (:ships state)))))

(defn- weave-side
  "Which way she is snaking right now: flipped halfway through the foe's
  reload, so the cross-range speed he led on when he fired is not the one she
  is carrying when the shell arrives. Nil threat means nobody is shooting at
  her and she can steady up."
  [threat]
  (cond (nil? threat) 0.0
        (> threat (* 0.5 FIRE-COOLDOWN)) 1.0
        :else -1.0))

(defn ai-course
  "The course an AI ship wants to be steering, as a unit [x z].

  Underneath is a circle of STANDOFF radius about the foe: the tangent,
  leaned on or off the bearing by however far the range is out, going round
  whichever way she is already going. On top of that she snakes either side
  of it while a gun is on her, because a shell is aimed where she would be if
  she kept doing what she is doing - so she makes sure she is not."
  [state id]
  (let [s (get-in state [:ships id])
        foe (live-foe state id)]
    (if (nil? foe)
      (let [[bx _ bz] (bow-dir s)] [bx bz])
      (let [[sx _ sz] (:pos s)
            [fx _ fz] (:pos foe)
            [bx _ bz] (bow-dir s)
            d (max 1e-6 (flat-range (:pos s) (:pos foe)))
            lx (/ (- fx sx) d) lz (/ (- fz sz) d)
            ;; keep circling the way she already is, so she commits to one
            ;; hand rather than dithering across the bearing
            sense (if (>= (+ (* bx (- lz)) (* bz lx)) 0.0) 1.0 -1.0)
            [vx _ vz] (or (:vel s) [0.0 0.0 0.0])
            closing (+ (* vx lx) (* vz lz))
            lean (max (- RANGE-LEAN-MAX)
                      (min RANGE-LEAN-MAX
                           (- (* RANGE-GAIN (- d STANDOFF))
                              (* RANGE-DAMP closing))))
            wx (+ (* sense (- lz)) (* lean lx))
            wz (+ (* sense lx) (* lean lz))
            m (Math/sqrt (+ (* wx wx) (* wz wz)))
            ;; The snake costs range: a course swung 35 degrees off the
            ;; tangent is half-radial. So it gives way to station-keeping in
            ;; proportion to how far out of position she is - full weave on
            ;; station, barely any of it on the run in.
            a (* WEAVE-ANGLE (weave-side (gun-threat s foe))
                 (/ 1.0 (+ 1.0 (Math/abs lean))))
            c (Math/cos a) sn (Math/sin a)
            ux (/ wx m) uz (/ wz m)]
        [(- (* ux c) (* uz sn))
         (+ (* ux sn) (* uz c))]))))

(defn ai-helm
  "Helm order for an AI ship: [thrust turn], right-positive.

  She is under way whenever she is afloat and has a foe - a ship holding
  station is a ship with a solved firing problem. The helm steers her onto
  ai-course, and while a loaded gun is pointed at her, or she is close
  enough that she cannot dodge at all, it stays hard over rather than
  settling on the wanted heading."
  [state id]
  (let [s (get-in state [:ships id])
        foe (live-foe state id)]
    (if (or (nil? foe) (:sunk s) (not (:ai s)))
      [0.0 0.0]
      (let [[wx wz] (ai-course state id)
            [bx _ bz] (bow-dir s)
            off (- (* bz wx) (* bx wz))
            t (gun-threat s foe)
            pressed? (or (and t (< t THREAT-HORIZON))
                         (< (flat-range (:pos s) (:pos foe)) KNIFE-RANGE))]
        [1.0 (cond
               (> off HELM-DEADBAND) 1.0
               (< off (- HELM-DEADBAND)) -1.0
               ;; already on the wanted heading but still under the gun:
               ;; keep the helm working rather than steadying into the
               ;; solution he has already computed
               pressed? (let [w (weave-side t)] (if (zero? w) 1.0 w))
               :else 0.0)]))))

(defn flight-time
  "How long a low-arc shell is actually in the air over a flat range d:
  d / (v cos theta), not d / v. The difference is a fifth of the flight at
  fighting range, which is several hull-widths of lead."
  [d]
  (let [s2 (* SHELL-SPEED SHELL-SPEED)
        disc (- (* s2 s2) (* SHELL-GRAVITY SHELL-GRAVITY d d))]
    (if (or (< d 1e-6) (neg? disc))
      (/ (max d 1e-6) SHELL-SPEED)
      (let [tan (/ (- s2 (Math/sqrt disc)) (* SHELL-GRAVITY d))
            cos (/ 1.0 (Math/sqrt (+ 1.0 (* tan tan))))]
        (/ d (* SHELL-SPEED cos))))))

(defn aim-point
  "Where an AI gunner aims: where the foe will be when the shell arrives,
  raised to hull height. Solved by iteration, because the lead moves the
  target, which moves the range, which moves the time of flight."
  [shooter foe]
  (let [[sx _ sz] (:pos shooter)
        [tx ty tz] (:pos foe)
        [vx _ vz] (or (:vel foe) [0.0 0.0 0.0])
        reach (fn [t]
                (let [px (+ tx (* vx t)) pz (+ tz (* vz t))]
                  [px pz (flat-range [px 0.0 pz] [sx 0.0 sz])]))]
    (loop [t (/ (flat-range [tx 0.0 tz] [sx 0.0 sz]) SHELL-SPEED) n 0]
      (let [[px pz d] (reach t)
            t' (flight-time d)]
        (if (or (= n 4) (< (Math/abs (- t' t)) 1e-4))
          [px (+ ty 1.0) pz]
          (recur t' (inc n)))))))

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
                                               :speed (or (:speed f) 0.0)
                                               :displaced (or (:displaced f) 0.0))]
                              (if (and (not (:sunk moved))
                                       (< (second (:pos f)) (- SUNK-DEPTH)))
                                (assoc moved :sunk true)
                                moved))
                            s)]))
                  ships))))

(def ^:private WAKE-STATIONS
  "Where along the keel a hull is coupled to the water, as a fraction of its
  half-length. One disc at the anchor leaves most of a 26-unit hull touching
  nothing; three make the coupling the shape of the ship."
  [-0.7 0.0 0.7])
;; the stations share one hull between them, so three of them displace one
;; ship's worth of water rather than three
(def ^:private WAKE-SHARE (/ 1.0 (double (count WAKE-STATIONS))))
;; equivalent-area radius of the footprint one station stands for, and how
;; far out its influence reaches (the displacement profile is negligible
;; past three of these)
(def ^:private STATION-RADIUS
  (Math/sqrt (/ (* ship/BEAM ship/LENGTH WAKE-SHARE) Math/PI)))
(def ^:private STATION-REACH (* 3.0 STATION-RADIUS))

;; gains, all rates: the water is forced for dt seconds, never kicked once
;; per frame, so none of this changes when the frame rate does
(def ^:private DISPLACE-GAIN 0.35) ; surface fall per unit of mean draft
(def ^:private BOW-WAVE-GAIN 0.40) ; bow wave / stern trough per unit speed
(def ^:private FLOW-GAIN 0.10)     ; water shoved aside per unit speed
(def ^:private SWIRL-GAIN 0.40)    ; vorticity shed per unit speed

(defn- hulls-of
  "What each floating hull does to the water, at stations along its keel.

  She displaces water simply by being there - `displace` is her mean draft,
  the divergence-theorem displaced volume over her footprint, so a hull
  taking on water sits deeper and a shot-up one sits higher - and displaces
  more of it by moving through it: a bow wave, a stern trough, water shoved
  out of her path and closing in astern, and vorticity shed off both sides of
  her track. hx/hz is her heading, which signs all three of those."
  [st]
  (for [[_ s] (:ships st)
        :when (not (:sunk s))
        :let [v (or (:speed s) 0.0)
              draft (/ (or (:displaced s) 0.0) (* ship/BEAM ship/LENGTH))
              [bx _ bz] (bow-dir s)
              [vx _ vz] (or (:vel s) [0.0 0.0 0.0])
              sp (Math/sqrt (+ (* vx vx) (* vz vz)))
              ;; heading is the course made good while she has way on,
              ;; otherwise where her bow points
              [hx hz] (if (> sp 1e-3) [(/ vx sp) (/ vz sp)] [bx bz])
              half (* 0.5 ship/LENGTH)]
        f WAKE-STATIONS]
    {:x (+ ((:pos s) 0) (* bx f half))
     :z (+ ((:pos s) 2) (* bz f half))
     :r STATION-REACH
     :hull-r STATION-RADIUS
     :displace (* DISPLACE-GAIN draft)
     :push (* FLOW-GAIN v)
     :lift (* BOW-WAVE-GAIN v)
     :swirl (* SWIRL-GAIN v)
     :hx hx :hz hz}))

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
