(ns voxel.physics
  "Box3D orchestration for the naval game. Owns the physics world plus the
  ball and every vessel/rubble body, steps the simulation, and reports
  plain-data facts for voxel.world/apply-physics to fold in. Impure boundary -
  all game decisions (blast, split, shatter, scoring) stay in the pure
  voxel.world.

  The ocean world has no ground: the sea surface at y = 0 is where all the
  interesting physics happens. Every body whose record carries cells gets
  divergence-theorem buoyancy (voxel.buoyancy) applied per step at its centre
  of buoyancy, plus the weight of any water taken on through breaches. A body
  floats on its recorded cell mesh - which damage-cells! can shrink without
  touching the collision shape, so flooding and buoyancy react to damage the
  instant it happens while the shape refresh stays with the world layer."
  (:require [voxel.box3d :as b3]
            [voxel.hullc :as hullc]
            [voxel.mesh :as mesh]
            [voxel.ocean :as ocean]
            [voxel.seac :as seac]
            [voxel.buoyancy :as buoy]
            [voxel.world :as w]))

(def ^:private CELL-DENSITY 1.8)
(def ^:private CELL-FRICTION 0.8)
(def ^:private CELL-RESTITUTION 0.05)
(def ^:private BALL-DENSITY 61.1)
(def ^:private BALL-FRICTION 0.4)
(def ^:private BALL-RESTITUTION 0.3)
(def ^:private SUBSTEPS 8)

;; A hull is not rubble. The world's default damping is tuned to bleed
;; micro-rotation out of debris so it reaches sleep; left on a ship it
;; swallows the helm. Yaw damping now comes from the water instead (see
;; drive-hydrodynamics! below), so the body's own angular damping only has to
;; catch what that misses. Linear damping stays put - that is the resistance
;; along the keel that caps her speed.
(def HULL-LINEAR-DAMPING 0.6)
(def HULL-ANGULAR-DAMPING 0.05)

;; A hull resists moving sideways far harder than it resists moving ahead:
;; that is what a keel is for, and it is the whole reason a turn changes
;; where a ship ENDS UP rather than just which way she points. Without it the
;; body slides sideways as easily as forward, so putting the helm over swung
;; the bow while momentum carried her along the old track - a ship at full
;; helm left her straight-line course by well under a metre in a shell's
;; flight, and could not be missed.
;;
;; Applied at a bow and a stern station against the LOCAL water speed there,
;; which includes the rotational part, so the same drag that stops sideslip
;; also damps yaw and weathervanes her onto her course.
;;
;; Both the strength and the stations scale with the hull. A fixed coefficient
;; would give a raft a dreadnought's grip on the water, and stations at a
;; fixed arm would put a small body's drag outside its own length, which
;; pins it rigid - a test raft stopped heeling on a sloped sea entirely.
(def LATERAL-DRAG 1.28)          ; per unit of sideways speed, per unit hull

;; A hull dragged up or down through water is resisted by it, and without
;; that a ship is a cork: pushed under, she came back up and swung for six
;; seconds with barely any decay. This is around six tenths of critical for
;; the heave mode, so she rides a swell and settles from a blast instead of
;; ringing. Taken at four stations round the centre of mass against the
;; LOCAL vertical speed, so the same drag damps pitch and roll with it.
(def HEAVE-DRAG 5.0)             ; per unit of vertical speed, per unit hull
(def ^:private DRAG-ARM 0.7)     ; station, as a fraction of the half-extent
(def ^:private EXPLOSION-FALLOFF 1.0)

(def ^:private world* (volatile! nil))
(def ^:private ball* (volatile! nil))
(def ^:private bodies* (volatile! {}))
;; hull ids into the native floatation kernel, which owns each body's
;; surface mesh so no triangle crosses the boundary per step
(def ^:private hulls* (volatile! {}))

(defn- claim-hull!
  []
  (let [used (set (vals @hulls*))]
    (first (remove used (range hullc/MAX-HULLS)))))

(defn- hull-record
  "Rebuild everything derived from a body's cells: the native surface mesh,
  the exposed faces, the footprint, the balance point, and the breach set
  the flooding reads. Runs on spawn and on damage, never per step."
  [hull cells anchor voxel skin]
  (let [{:keys [faces span com]} (hullc/set-hull! hull (keys cells) anchor voxel)
        fset (set faces)]
    {:faces fset
     :span span
     :com com
     :breaches (if skin (clojure.set/difference fset skin) #{})}))

(defn init!
  "Create the open-ocean Box3D world with the game's gravity: no ground, no
  walls - things that sink leave the arena. Any previous world is replaced."
  []
  (when-let [old @world*] (b3/destroy-world! old))
  ;; the C sparse-field kernel, when the native library loads: same sums as
  ;; the pure reference, microseconds instead of milliseconds per frame
  (try
    (seac/field [{:x 0.0 :z 0.0 :omega 0.0} {:x 1.0 :z 0.0 :omega 1.0}] 14.0 1e-9)
    (reset! ocean/field-kernel
            (fn [oc] (seac/field (:particles oc)
                                 ocean/FIELD-RADIUS ocean/ACTIVE-EPS)))
    (catch Exception _ nil))
  ;; the C multi-level FMM kernel: the same field as the pure FMM at
  ;; native speed, for the fully-churned ambient ocean
  (try
    (seac/fmm [{:x 0.0 :z 0.0 :omega 0.0} {:x 1.0 :z 0.0 :omega 1.0}]
              10 ocean/BOUNDS)
    (reset! ocean/fmm-kernel
            (fn [oc] (seac/fmm (:particles oc) (or (:p oc) 10) (:bounds oc)
                               (or (:cx oc) 0.0) (or (:cz oc) 0.0))))
    (catch Exception _ nil))
  ;; the native particle sim: the whole ocean state in flat C arrays, so no
  ;; particle crosses the FFI boundary in the frame loop
  (try
    (seac/sim-init! 4 1.0 2.0 false 0.0 2048)
    (seac/sim-free!)
    (reset! ocean/sim-kernel
            {:init! seac/sim-init!
             :step! seac/sim-step!
             :particles seac/sim-particles
             :time seac/sim-time
             :height seac/sim-height
             :recenter! seac/sim-recenter!
             :origin seac/sim-origin})
    (catch Exception _ nil))
  (let [wrld (b3/create-world 0.0 (- w/GRAVITY) 0.0 1)]
    (vreset! world* wrld)
    (vreset! ball* nil)
    (doseq [h (vals @hulls*)] (hullc/free-hull! h))
    (vreset! hulls* {})
    (vreset! bodies* {})
    wrld))

(defn spawn-ball!
  "Create the cannonball body at origin with velocity v. One live ball."
  [[ox oy oz] [vx vy vz]]
  (when-let [old @ball*] (b3/destroy-body! old))
  (let [id (b3/create-ball @world* ox oy oz w/BALL-RADIUS vx vy vz
                           BALL-DENSITY BALL-FRICTION BALL-RESTITUTION)]
    (vreset! ball* id)
    id))

(defn spawn-body!
  "Create a dynamic compound body: one unit box per cell, offset relative to
  `anchor`, posed at pos/quat. The cell set and its spawn-time skin are
  recorded so buoyancy and flooding always read the live mesh. awake 0 spawns
  it sleeping. vel (optional [vx vy vz]) carries impact velocity over."
  ([pos quat awake anchor cells]
   (spawn-body! pos quat awake anchor cells nil 1.0))
  ([pos quat awake anchor cells vel]
   (spawn-body! pos quat awake anchor cells vel 1.0))
  ([pos quat awake anchor cells vel voxel]
   (let [[ax ay az] anchor
         [px py pz] pos
         [qx qy qz qw] quat
         s (double voxel)
         live (zipmap cells (repeat :cell))
         id (b3/create-body @world* b3/DYNAMIC-BODY px py pz qx qy qz qw awake)]
     ;; One collision solid per voxel does not survive a fine grid - a hull
     ;; is thousands of cells and a handful of merged boxes. They tile the
     ;; cells exactly, so the body still has the voxels' mass and shape;
     ;; each is shaved a hair so touching hulls cannot grind coplanar faces.
     (doseq [[i0 j0 k0 i1 j1 k1] (mesh/solid-boxes live)]
       (b3/add-box! id
                    (* s (- (* 0.5 (+ i0 i1)) ax))
                    (* s (- (* 0.5 (+ j0 j1)) ay))
                    (* s (- (* 0.5 (+ k0 k1)) az))
                    (- (* 0.5 s (- i1 i0)) 0.01)
                    (- (* 0.5 s (- j1 j0)) 0.01)
                    (- (* 0.5 s (- k1 k0)) 0.01)
                    CELL-DENSITY CELL-FRICTION CELL-RESTITUTION))
     (b3/set-damping! id HULL-LINEAR-DAMPING HULL-ANGULAR-DAMPING)
     (when vel
       (b3/set-velocity! id (vel 0) (vel 1) (vel 2)))
     (let [hull (claim-hull!)
           skin (set (:faces (hullc/set-hull! hull (keys live) anchor s)))]
       (vswap! hulls* assoc id hull)
       (vswap! bodies* assoc id
               (merge {:cells live
                       :anchor anchor
                       :voxel s
                       :volume (* (count live) s s s)
                       :hull hull
                       :skin skin
                       :flood 0.0}
                      (hull-record hull live anchor s skin))))
     id)))

(defn damage-cells!
  "Remove cells from a body's recorded mesh (destroyed by blasts). The
  spawn-time skin stays put, so the faces this exposes below the waterline
  count as intake openings and the hull starts taking on water immediately.
  The collision shape is untouched - the world layer owns body lifecycles."
  [id cells]
  (when-let [rec (get @bodies* id)]
    (let [live' (reduce dissoc (:cells rec) cells)]
      (vswap! bodies* update id
              (fn [b]
                (merge b
                       {:cells live'
                        :volume (* (count live') (:voxel b) (:voxel b)
                                   (:voxel b))}
                       ;; damage moves the balance point too, and driving her
                       ;; from the old one is what makes a wreck wander
                       (hull-record (:hull b) live' (:anchor b) (:voxel b)
                                    (:skin b))))))))

(def ENGINE-FORCE 10100.0)  ; ~8 u/s flat out against the linear damping
(def RUDDER-FORCE 5750.0)   ; bow/stern couple, ~25 deg/s of yaw
(def ^:private RUDDER-ARM 11.0)      ; half the keel, about the anchor

(defn steer!
  "Helm command for one step: thrust -1..1 drives the hull along its bow
  axis (local +k, the bow) through her centre of mass, turn -1..1 yaws her
  via a rudder force couple at bow and stern (right helm swings the bow to
  starboard, +x at identity yaw)."
  [id thrust turn]
  (when-let [rec (get @bodies* id)]
    (let [[pos quat] (b3/transform id)
          [px py pz] pos
          com (buoy/q-rotate quat (or (:com rec) [0.0 0.0 0.0]))
          fwd (buoy/q-rotate quat [0.0 0.0 1.0])
          push (fn [f p]
                 (b3/apply-force! id (f 0) (f 1) (f 2)
                                  (+ px (com 0) (p 0))
                                  (+ py (com 1) (p 1))
                                  (+ pz (com 2) (p 2))
                                  true))]
      (when (not (zero? thrust))
        (push (mapv #(* thrust ENGINE-FORCE %) fwd) [0.0 0.0 0.0]))
      (when (not (zero? turn))
        (let [side (buoy/q-rotate quat [1.0 0.0 0.0])
              f (mapv #(* turn RUDDER-FORCE %) side)
              bow (buoy/q-rotate quat [0.0 0.0 RUDDER-ARM])
              stern (buoy/q-rotate quat [0.0 0.0 (- RUDDER-ARM)])]
          (push f bow)
          (push (mapv - f) stern))))))

(defn explode!
  "Radial impulse at [x y z] reaching `radius` (decaying to zero over
  EXPLOSION-FALLOFF beyond it)."
  [[x y z] radius strength]
  (b3/explode! @world* x y z radius EXPLOSION-FALLOFF strength))

(defn sleep-body!
  "Force a body to sleep - Box3D settles its whole touching island. Used when
  the pure world declares a body settled after a long sub-threshold streak."
  [id]
  (b3/set-awake! id false))

(defn destroy-unreferenced!
  "Destroy physics bodies the world state no longer references (shattered or
  split-away parents, despawned balls)."
  [live-ids]
  (doseq [id (keys @bodies*)]
    (when-not (contains? live-ids id)
      (b3/destroy-body! id)
      (when-let [h (get @hulls* id)] (hullc/free-hull! h))
      (vswap! hulls* dissoc id)
      (vswap! bodies* dissoc id)))
  (when (and @ball* (not (contains? live-ids @ball*)))
    (b3/destroy-body! @ball*)
    (vreset! ball* nil)))

(defn- pose-record
  "The pure-floatation view of a live body: recorded mesh under the live pose."
  [id rec]
  (let [[pos quat] (b3/transform id)]
    {:pos pos
     :quat quat
     :anchor (:anchor rec)
     :voxel (:voxel rec)
     :cells (:cells rec)
     :skin (:skin rec)
     :faces (:faces rec)
     :breaches (:breaches rec)
     :span (:span rec)
     :com (:com rec)
     :flood (:flood rec)}))

(def ^:private WATER-SAMPLES
  "Where the water under a hull is read, as fractions of its own span along
  [beam, keel]. Centre plus the four extremes: enough for the least-squares
  fit to see the wave slope along the hull rather than just its heave."
  [[0.0 0.0] [1.0 0.0] [-1.0 0.0] [0.0 1.0] [0.0 -1.0]])

(defn- water-plane
  "The plane of the sea under one hull, fitted to the particle surface at
  WATER-SAMPLES points across its footprint. With no ocean wired (tests,
  tools) this is the still waterline, which is what it always used to be."
  [body water]
  (if (nil? water)
    buoy/WATER-LEVEL
    (let [[ex _ ez] (or (:span body) [4.0 1.0 13.0])
          quat (:quat body)
          [px _ pz] (:pos body)]
      (buoy/fit-water-plane
       (mapv (fn [[fi fk]]
               (let [[dx _ dz] (buoy/q-rotate quat [(* fi ex) 0.0 (* fk ez)])
                     x (+ px dx)
                     z (+ pz dz)]
                 [x (water x z) z]))
             WATER-SAMPLES)))))

(defn- drive-hydrodynamics!
  "The water's grip on one hull for this step: drag across the beam, and drag
  against moving up or down through the water. Nothing along the keel - the
  body's linear damping is already that.

  Both are taken at stations round the centre of mass against the LOCAL water
  speed there, which includes the rotational part, so the same two mechanisms
  also damp yaw, pitch and roll and weathervane her onto her course."
  [id rec]
  (let [[pos quat] (b3/transform id)
        [cx cy cz] (buoy/q-rotate quat (or (:com rec) [0.0 0.0 0.0]))
        [px py pz] (mapv + pos [cx cy cz])
        [vx vy vz] (b3/velocity id)
        [wx wy wz] (b3/angular-velocity id)
        side (buoy/q-rotate quat [1.0 0.0 0.0])
        fwd (buoy/q-rotate quat [0.0 0.0 1.0])
        ;; scaled by how much ship there is, not how many cells she is cut
        ;; into - a finer grid is the same vessel
        hull (:volume rec 1.0)
        [ex _ ez] (or (:span rec) [1.0 1.0 1.0])
        lat (* LATERAL-DRAG hull)
        ;; the vertical term is shared between its four stations, so the
        ;; total resistance is a property of the hull, not of how many
        ;; places it happens to be sampled at
        heave (* 0.25 HEAVE-DRAG hull)
        stations (concat (for [a [(* DRAG-ARM ez) (- (* DRAG-ARM ez))]]
                           [(* a (fwd 0)) (* a (fwd 2)) true])
                         (for [a [(* DRAG-ARM ex) (- (* DRAG-ARM ex))]]
                           [(* a (side 0)) (* a (side 2)) false]))]
    (doseq [[rx rz keel?] stations]
      ;; u = v + w x r; level with the centre of mass that comes out as
      ;; (wy rz, wz rx - wx rz, -wy rx). Get a sign backwards and the drag
      ;; drives the motion instead of damping it.
      (let [ux (+ vx (* wy rz))
            uz (- vz (* wy rx))
            uy (+ vy (- (* wz rx) (* wx rz)))]
        ;; sideslip needs only the two keel stations; taking it across the
        ;; beam as well would just double it
        (when keel?
          (let [slip (+ (* ux (side 0)) (* uz (side 2)))
                f (- (* lat slip))]
            (b3/apply-force! id (* f (side 0)) 0.0 (* f (side 2))
                             (+ px rx) py (+ pz rz) false)))
        (b3/apply-force! id 0.0 (- (* heave uy)) 0.0
                         (+ px rx) py (+ pz rz) false)))))

(defn- drive-floatation!
  "One body's water interaction for this step: buoyant uplift and flood weight
  applied as forces (waking the body - a settled hull must still ride every
  wave), and flood accumulated through whatever breaches are submerged.

  All three read the same plane, fitted to the particle surface under this
  hull, so a ship heaves on the swell, the slope of the wave under her rolls
  and pitches her, and a sea running over a breach floods her faster."
  [id rec dt water]
  (let [body (pose-record id rec)
        plane (buoy/as-plane (water-plane body water))
        ;; the divergence-theorem solve runs in C over the hull's own mesh
        {:keys [volume centroid]} (hullc/metrics (:hull rec) (:pos body)
                                                 (:quat body) plane)
        lift (when (and (pos? volume) centroid)
               {:force [0.0 (* buoy/WATER-DENSITY buoy/GRAVITY volume) 0.0]
                :point centroid
                :volume volume})]
    (doseq [{:keys [force point]} (keep identity
                                        [lift (buoy/flood-force body plane)])]
      (b3/apply-force! id
                       (force 0) (force 1) (force 2)
                       (point 0) (point 1) (point 2)
                       true))
    (vswap! bodies* update id assoc
            :flood (:flood (buoy/step-flooding body dt plane))
            ;; the water this hull is standing in, which the ocean needs to
            ;; know to displace it. Already solved for the uplift above.
            :displaced (or (:volume lift) 0.0))))

(defn- body-fact
  [id]
  (let [[pos quat] (b3/transform id)
        [vx vy vz] (b3/velocity id)]
    {:body id
     :pos pos
     :quat quat
     :vel [vx vy vz]
     :speed (Math/sqrt (+ (* vx vx) (* vy vy) (* vz vz)))
     :displaced (get-in @bodies* [id :displaced] 0.0)
     ;; the live exposed faces, so the renderer's hull mesh follows damage
     ;; without the world layer walking the cell set to find them
     :faces (get-in @bodies* [id :faces])
     :asleep (not (b3/awake? id))}))

(defn step!
  "Advance physics by dt seconds - floatation forces first, then the solver -
  and report one fact per live body. `water` is (fn [x z] -> surface height)
  from voxel.ocean/surface-fn; with none the sea is the still waterline."
  ([dt] (step! dt nil))
  ([dt water]
  (doseq [[id rec] @bodies*]
    (drive-hydrodynamics! id rec)
    (drive-floatation! id rec dt water))
  (b3/step! @world* dt SUBSTEPS)
  {:ball (when-let [id @ball*] (body-fact id))
   :bodies (mapv body-fact (keys @bodies*))}))
