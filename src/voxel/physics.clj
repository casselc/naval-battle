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
(def ^:private EXPLOSION-FALLOFF 1.0)

(def ^:private world* (volatile! nil))
(def ^:private ball* (volatile! nil))
(def ^:private bodies* (volatile! {}))

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
            (fn [oc] (seac/fmm (:particles oc)
                               (or (:p oc) 10) (:bounds oc))))
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
             :time seac/sim-time})
    (catch Exception _ nil))
  (let [wrld (b3/create-world 0.0 (- w/GRAVITY) 0.0 1)]
    (vreset! world* wrld)
    (vreset! ball* nil)
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
   (spawn-body! pos quat awake anchor cells nil))
  ([pos quat awake anchor cells vel]
   (let [[ax ay az] anchor
         [px py pz] pos
         [qx qy qz qw] quat
         id (b3/create-body @world* b3/DYNAMIC-BODY px py pz qx qy qz qw awake)]
     (doseq [[i j k] cells]
       ;; hulls are 0.98 wide (render size): a hair of daylight between
       ;; touching bodies stops coplanar face-grind without a visible gap
       (b3/add-box! id (- (+ i 0.5) ax) (- (+ j 0.5) ay) (- (+ k 0.5) az)
                    0.49 0.49 0.49 CELL-DENSITY CELL-FRICTION CELL-RESTITUTION))
     (when vel
       (b3/set-velocity! id (vel 0) (vel 1) (vel 2)))
     (let [live (zipmap cells (repeat :cell))
           {:keys [tris faces]} (buoy/surface-cache live anchor)]
       (vswap! bodies* assoc id {:cells live
                                 :anchor anchor
                                 :skin (buoy/skin-faces live)
                                 :tris tris
                                 :faces faces
                                 :flood 0.0}))
     id)))

(defn damage-cells!
  "Remove cells from a body's recorded mesh (destroyed by blasts). The
  spawn-time skin stays put, so the faces this exposes below the waterline
  count as intake openings and the hull starts taking on water immediately.
  The collision shape is untouched - the world layer owns body lifecycles."
  [id cells]
  (when-let [rec (get @bodies* id)]
    (vswap! bodies* update-in [id]
            (fn [live]
              (let [live' (reduce dissoc (:cells live) cells)
                    {:keys [tris faces]} (buoy/surface-cache live' (:anchor live))]
                (-> live
                    (assoc :cells live')
                    (assoc :tris tris)
                    (assoc :faces faces)))))))

(def ^:private ENGINE-FORCE 6000.0)  ; ~8 u/s flat out against 0.6 damping
(def ^:private RUDDER-FORCE 1500.0)  ; bow/stern couple, ~25 deg/s of yaw
(def ^:private RUDDER-ARM 11.0)      ; half the keel, about the anchor

(defn steer!
  "Helm command for one step: thrust -1..1 drives the hull along its bow
  axis (local +k, the bow), turn -1..1 yaws it via a rudder force couple at
  bow and stern (right helm swings the bow to starboard, +x at identity
  yaw). The default Box3D hull damping is the water resistance that caps
  both speed and turn rate."
  [id thrust turn]
  (when (get @bodies* id)
    (let [[pos quat] (b3/transform id)
          [px py pz] pos
          fwd (buoy/q-rotate quat [0.0 0.0 1.0])
          push (fn [f p]
                 (b3/apply-force! id (f 0) (f 1) (f 2)
                                  (+ px (p 0)) (+ py (p 1)) (+ pz (p 2)) true))]
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
     :cells (:cells rec)
     :skin (:skin rec)
     :tris (:tris rec)
     :faces (:faces rec)
     :flood (:flood rec)}))

(defn- drive-floatation!
  "One body's water interaction for this step: buoyant uplift and flood weight
  applied as forces (waking the body - a settled hull must still ride every
  wave), and flood accumulated through whatever breaches are submerged."
  [id rec dt]
  (let [body (pose-record id rec)]
    (doseq [{:keys [force point]} (keep #(% body) [buoy/buoyancy-force
                                                    buoy/flood-force])]
      (b3/apply-force! id
                       (force 0) (force 1) (force 2)
                       (point 0) (point 1) (point 2)
                       true))
    (vswap! bodies* assoc-in [id :flood]
            (:flood (buoy/step-flooding body dt)))))

(defn- body-fact
  [id]
  (let [[pos quat] (b3/transform id)
        [vx vy vz] (b3/velocity id)]
    {:body id
     :pos pos
     :quat quat
     :vel [vx vy vz]
     :speed (Math/sqrt (+ (* vx vx) (* vy vy) (* vz vz)))
     :asleep (not (b3/awake? id))}))

(defn step!
  "Advance physics by dt seconds - floatation forces first, then the solver -
  and report one fact per live body."
  [dt]
  (doseq [[id rec] @bodies*]
    (drive-floatation! id rec dt))
  (b3/step! @world* dt SUBSTEPS)
  {:ball (when-let [id @ball*] (body-fact id))
   :bodies (mapv body-fact (keys @bodies*))})
