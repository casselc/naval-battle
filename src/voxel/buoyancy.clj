(ns voxel.buoyancy
  "Buoyancy from the divergence theorem, per the plan: clip a body's voxel
  surface mesh at the water plane and read displaced volume and centre of
  buoyancy straight off the clipped triangles.

  Why clipping without capping is exact: any triangle lying IN the plane
  y = water-level contributes exactly zero to the divergence-theorem volume
  (its normal is vertical, and all its vertices share one y) and to every
  axis moment (w = 0 on the two horizontal axes; a = b = c = 0 on the
  vertical one). So the open mesh left by dropping the above-water triangles
  has the same volume and moments as the properly capped solid.

  Flooding: a body remembers its spawn-time exterior skin. Exposed faces
  that are NOT skin are fracture faces from destroyed cells; the ones below
  the waterline with a non-upward normal are intake openings. Water mass
  accumulates through them, applied as weight at the lowest cell - the ship
  settles, more openings submerge, and it lists and sinks from the breach.

  All pure math on plain body maps ({:cells :anchor :pos :quat :skin :flood})."
  (:require [voxel.mesh :as mesh]))

(def GRAVITY 25.0)
(def WATER-DENSITY 2.6)
(def WATER-LEVEL 0.0)
(def FLOOD-RATE 0.5)

;; --- quaternions ([x y z w]) ---------------------------------------------------

(defn q-rotate [[qx qy qz qw] v]
  (let [[vx vy vz] v
        ;; t = q_vec x v, u = t + w v, v' = v + 2 (q_vec x u)
        tx (- (* qy vz) (* qz vy))
        ty (- (* qz vx) (* qx vz))
        tz (- (* qx vy) (* qy vx))
        ux (+ tx (* qw vx))
        uy (+ ty (* qw vy))
        uz (+ tz (* qw vz))]
    [(+ vx (* 2.0 (- (* qy uz) (* qz uy))))
     (+ vy (* 2.0 (- (* qz ux) (* qx uz))))
     (+ vz (* 2.0 (- (* qx uy) (* qy ux))))]))

(defn yaw-quat [angle] [0.0 (Math/sin (/ angle 2.0)) 0.0 (Math/cos (/ angle 2.0))])
(defn pitch-quat [angle] [(Math/sin (/ angle 2.0)) 0.0 0.0 (Math/cos (/ angle 2.0))])

(defn body-point->world
  "Mesh/local point v (spawn-grid coords) under the body's live pose:
  world = pos + R(quat) (v - anchor)."
  [{:keys [pos quat anchor]} v]
  (let [[ax ay az] anchor
        d [(- (v 0) ax) (- (v 1) ay) (- (v 2) az)]
        [rx ry rz] (q-rotate quat d)]
    [(+ (pos 0) rx) (+ (pos 1) ry) (+ (pos 2) rz)]))

(defn cell-center->world
  "World centre of cell [i j k] under the body's pose."
  [body [i j k]]
  (body-point->world body [(+ i 0.5) (+ j 0.5) (+ k 0.5)]))

;; --- clipping at the water plane --------------------------------------------------

(defn- lerp-to-plane
  "Point where the segment a->b crosses y = y0 (a and b strictly on opposite
  sides)."
  [a b y0]
  (let [t (/ (- y0 (a 1)) (- (b 1) (a 1)))]
    [(+ (a 0) (* t (- (b 0) (a 0))))
     y0
     (+ (a 2) (* t (- (b 2) (a 2))))]))

(defn clip-below
  "Clip triangles to the half-space y <= y0, Sutherland-Hodgman against one
  plane. Returns 0, 1 or 2 triangles per input, winding preserved."
  [tris y0]
  (letfn [(clip-tri [[a b c]]
            (let [below? (fn [v] (<= (v 1) y0))
                  ba (below? a) bb (below? b) bc (below? c)]
              (cond
                (and ba bb bc) [[a b c]]
                (or (and ba bb) (and bb bc) (and bc ba))
                ;; two below: quad [below1 below2 edge1 edge0] in winding order
                (if (and ba bb)
                  (let [e0 (lerp-to-plane b c y0) e1 (lerp-to-plane a c y0)]
                    [[a b e0] [a e0 e1]])
                  (if (and bb bc)
                    (let [e0 (lerp-to-plane c a y0) e1 (lerp-to-plane b a y0)]
                      [[b c e0] [b e0 e1]])
                    (let [e0 (lerp-to-plane a b y0) e1 (lerp-to-plane c b y0)]
                      [[c a e0] [c e0 e1]])))
                (or ba bb bc)
                ;; one below: single corner triangle
                (if ba
                  [[a (lerp-to-plane a b y0) (lerp-to-plane a c y0)]]
                  (if bb
                    [[b (lerp-to-plane b c y0) (lerp-to-plane b a y0)]]
                    [[c (lerp-to-plane c a y0) (lerp-to-plane c b y0)]]))
                :else nil)))]
    (into [] (mapcat clip-tri) tris)))

;; --- submerged volume + centre of buoyancy -------------------------------------------

(defn world-surface-tris
  "The body's closed surface mesh transformed into live world coordinates."
  [body]
  (mapv (fn [[a b c]]
          [(body-point->world body a) (body-point->world body b) (body-point->world body c)])
        (mesh/surface-triangles (:cells body))))

(defn submerged-metrics
  "{:volume v :centroid [x y z]} of the part of the body below the water
  plane, or {:volume 0 :centroid nil} when dry. Volume and centroid are the
  divergence-theorem sums over the clipped open mesh (see ns doc)."
  ([body] (submerged-metrics body WATER-LEVEL))
  ([body water-y]
   (let [tris (clip-below (world-surface-tris body) water-y)
         v (mesh/mesh-volume tris)]
     (if (< (Math/abs v) 1e-9)
       {:volume 0.0 :centroid nil}
       {:volume v :centroid (mesh/mesh-centroid tris)}))))

(defn buoyancy-force
  "Archimedes: rho g V displaced, straight up, applied at the centre of
  buoyancy so the offset from the centre of mass rights the ship. nil when
  nothing is submerged."
  ([body] (buoyancy-force body WATER-LEVEL))
  ([body water-y]
   (let [{:keys [volume centroid]} (submerged-metrics body water-y)]
     (when (and (pos? volume) centroid)
       {:force [0.0 (* WATER-DENSITY GRAVITY volume) 0.0]
        :point centroid}))))

;; --- skin, breach openings, flooding --------------------------------------------------

(def ^:private DIRS
  {[1 0 0] 1.0 [-1 0 0] 1.0 [0 1 0] 0.0 [0 -1 0] -1.0 [0 0 1] 1.0 [0 0 -1] 1.0})

(defn surface-faces
  "Currently exposed faces as [cell dir] pairs."
  [cells]
  (into #{}
        (mapcat (fn [cell]
                  (keep #(when-not (contains? cells (mapv + cell (key %)))
                           [cell (key %)])
                         DIRS)))
        (keys cells)))

(defn skin-faces
  "Exterior faces of an intact hull - the watertight skin recorded at spawn.
  Bodies that split later keep the original skin, so new exposed faces are
  recognisable as breach openings."
  [cells]
  (surface-faces cells))

(defn- face-center
  "Face centre in world coordinates: cell centre + dir/2, posed."
  [body [cell dir]]
  (body-point->world body (mapv (fn [c d] (+ c 0.5 (* 0.5 d))) cell dir)))

(defn- face-world
  "Face centre and outward normal in world coordinates."
  [body [cell dir]]
  {:center (face-center body [cell dir])
   :normal (q-rotate (:quat body) (vec dir))})

(defn openings-below
  "Count of intake openings: exposed fracture faces (not original skin) whose
  centre is below the waterline and whose normal does not point sharply up.
  A body with no recorded skin is watertight (loose rubble, debris)."
  ([body] (openings-below body WATER-LEVEL))
  ([{:keys [skin] :as body} water-y]
   (if (nil? skin)
     0
     (let [breaches (clojure.set/difference (surface-faces (:cells body)) skin)]
       (count (filter (fn [f]
                       (let [{:keys [center normal]} (face-world body f)]
                         (and (< (center 1) water-y)
                              (<= (normal 1) 0.25))))
                     breaches))))))

(defn step-flooding
  "Accumulate ingressed water for dt seconds: FLOOD-RATE per submerged
  opening, capped at one unit per cell of hull."
  ([body dt] (step-flooding body dt WATER-LEVEL))
  ([body dt water-y]
   (let [cap (count (:cells body))
         rate (* FLOOD-RATE (openings-below body water-y))]
     (assoc body :flood (min (double cap)
                             (+ (or (:flood body) 0.0) (* rate dt)))))))

(defn flood-force
  "Weight of the water taken on: rho g flood straight down, applied at the
  lowest cell centre so the ship trims bow/stern and lists toward the hole.
  nil when the hull is dry."
  [body]
  (let [f (or (:flood body) 0.0)]
    (if (<= f 0.0)
      nil
      (let [lowest (first (sort-by (fn [c] ((cell-center->world body c) 1))
                                   (keys (:cells body))))
            p (cell-center->world body lowest)]
        {:force [0.0 (- (* WATER-DENSITY GRAVITY f)) 0.0]
         :point p}))))
