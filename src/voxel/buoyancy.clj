(ns voxel.buoyancy
  "Buoyancy from the divergence theorem, per the plan: clip a body's voxel
  surface mesh at the water plane and read displaced volume and centre of
  buoyancy straight off the clipped triangles.

  The water plane is [nx ny nz d] in world coordinates - a point p is under
  water when n.p <= d - because the sea is a particle field, not a sheet at
  y = 0. voxel.physics fits it to the particles under each hull every step,
  so a ship heaves on the swell and the slope of the wave under it rights or
  rolls her. A bare number still works and means the horizontal plane at
  that height.

  Why clipping without capping is exact: any triangle lying IN the water
  plane contributes exactly zero to the divergence-theorem volume (its
  normal is the plane normal, and all its vertices share one height) and to
  every axis moment (w = 0 on the two in-plane axes; a = b = c = 0 on the
  one along the normal). So the open mesh left by dropping the above-water
  triangles has the same volume and moments as the properly capped solid.
  That argument needs the cap to lie in the coordinate plane, so the sums
  run in a frame whose up axis IS the water plane normal: volume is
  invariant under that rotation and the centroid comes back through it.

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

;; --- the water plane ---------------------------------------------------------------

(defn- cross
  [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn- dot3 [a b] (+ (* (a 0) (b 0)) (* (a 1) (b 1)) (* (a 2) (b 2))))

(defn as-plane
  "The water plane as [nx ny nz d] with n a unit normal pointing out of the
  water: a point p is submerged when n.p <= d. A bare number is the
  horizontal plane at that height."
  [p]
  (if (number? p) [0.0 1.0 0.0 (double p)] p))

(defn fit-water-plane
  "Least-squares plane through sampled surface points [[x y z] ...], as
  [nx ny nz d]. Fits y = a x + b z + c about the sample centroid, which
  keeps the 2x2 normal equations well conditioned; a degenerate pattern (all
  samples over one spot, or along a single line) has no recoverable slope
  and falls back to the horizontal plane at the mean height."
  [pts]
  (let [pts (vec pts)
        n (count pts)
        inv (/ 1.0 (double (max 1 n)))
        mx (* inv (reduce + (map #(% 0) pts)))
        my (* inv (reduce + (map #(% 1) pts)))
        mz (* inv (reduce + (map #(% 2) pts)))
        [sxx sxz szz sxy szy]
        (reduce (fn [[axx axz azz axy azy] p]
                  (let [x (- (p 0) mx) y (- (p 1) my) z (- (p 2) mz)]
                    [(+ axx (* x x)) (+ axz (* x z)) (+ azz (* z z))
                     (+ axy (* x y)) (+ azy (* z y))]))
                [0.0 0.0 0.0 0.0 0.0] pts)
        det (- (* sxx szz) (* sxz sxz))
        [a b] (if (< (Math/abs det) 1e-12)
                [0.0 0.0]
                [(/ (- (* sxy szz) (* szy sxz)) det)
                 (/ (- (* szy sxx) (* sxy sxz)) det)])
        c (- my (* a mx) (* b mz))
        l (Math/sqrt (+ (* a a) 1.0 (* b b)))]
    [(/ (- a) l) (/ 1.0 l) (/ (- b) l) (/ c l)]))

(defn- plane-frame
  "Rows of a rotation taking the water plane normal n onto +y, so the
  clipped cap lies in the frame's y = 0 plane. Identity when the water is
  already level, which keeps flat-sea results bit-for-bit unchanged."
  [[nx ny nz :as n]]
  (if (> ny 0.999999)
    [[1.0 0.0 0.0] [0.0 1.0 0.0] [0.0 0.0 1.0]]
    (let [a (if (< (Math/abs ny) 0.9) [0.0 1.0 0.0] [1.0 0.0 0.0])
          e (cross a n)
          l (Math/sqrt (dot3 e e))
          e1 (mapv #(/ % l) e)]
      ;; rows [e1 n e1 x n]: orthonormal AND right-handed, so the
      ;; divergence sums keep their sign
      [e1 (vec n) (cross e1 n)])))

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

(defn- local-water-plane
  "The water plane expressed in anchor-relative body coordinates:
  [nx ny nz d] with a body point u submerged iff n.u <= d. Rigid transforms
  preserve volume, so clipping runs on the static local mesh and only the
  surviving triangles rotate."
  [{:keys [pos quat]} water]
  (let [[wx wy wz d] (as-plane water)
        [qx qy qz qw] quat
        n (q-rotate [(- qx) (- qy) (- qz) qw] [wx wy wz])]
    [(n 0) (n 1) (n 2) (- d (+ (* wx (pos 0)) (* wy (pos 1)) (* wz (pos 2))))]))

(defn- quat-matrix
  "The 3x3 rotation matrix of [x y z w] - rows map local u to world axes."
  [[qx qy qz qw]]
  (let [xx (* qx qx) yy (* qy qy) zz (* qz qz)
        xy (* qx qy) xz (* qx qz) yz (* qy qz)
        wz (* qw qz) wy (* qw qy) wx (* qw qx)]
    [[(- 1.0 (* 2.0 (+ yy zz))) (* 2.0 (- xy wz)) (* 2.0 (+ xz wy))]
     [(* 2.0 (+ xy wz)) (- 1.0 (* 2.0 (+ xx zz))) (* 2.0 (- yz wx))]
     [(* 2.0 (- xz wy)) (* 2.0 (+ yz wx)) (- 1.0 (* 2.0 (+ xx yy)))]]))

(defn submerged-metrics
  "{:volume v :centroid [x y z]} of the part of the body below the water
  plane, or {:volume 0 :centroid nil} when dry. Volume and centroid are the
  divergence-theorem sums over the clipped open mesh (see ns doc).

  Fast path: clip the static anchor-relative mesh against the local water
  plane (a dry triangle costs three dot products and no allocation), rotate
  only the survivors, and shift the plane to y = 0 before summing so the
  open-mesh identities hold exactly."
  ([body] (submerged-metrics body WATER-LEVEL))
  ([{:keys [pos quat anchor cells] :as body} water]
   (let [us (or (:tris body)
                (mapv (fn [t] (mapv #(mapv - % anchor) t))
                      (mesh/surface-triangles cells)))
         [wx wy wz wd] (as-plane water)
         [nx ny nz d] (local-water-plane body water)
         ;; signed height of the body origin above the water plane, so the
         ;; cap lands on y = 0 in the plane frame
         dy (- wd (+ (* wx (pos 0)) (* wy (pos 1)) (* wz (pos 2))))
         s (fn [u] (+ (* nx (u 0)) (* ny (u 1)) (* nz (u 2))))
         clip (fn [[a b c :as t]]
                (let [sa (s a) sb (s b) sc (s c)
                      ba (<= sa d) bb (<= sb d) bc (<= sc d)
                      lerp (fn [p q sp sq]
                             (let [tt (/ (- d sp) (- sq sp))]
                               [(+ (p 0) (* tt (- (q 0) (p 0))))
                                (+ (p 1) (* tt (- (q 1) (p 1))))
                                (+ (p 2) (* tt (- (q 2) (p 2))))]))]
                  (cond
                    (and ba bb bc) [t]
                    (or (and ba bb) (and bb bc) (and bc ba))
                    (if (and ba bb)
                      (let [e0 (lerp b c sb sc) e1 (lerp a c sa sc)]
                        [[a b e0] [a e0 e1]])
                      (if (and bb bc)
                        (let [e0 (lerp c a sc sa) e1 (lerp b a sb sa)]
                          [[b c e0] [b e0 e1]])
                        (let [e0 (lerp a b sa sb) e1 (lerp c b sc sb)]
                          [[c a e0] [c e0 e1]])))
                    (or ba bb bc)
                    (if ba
                      [[a (lerp a b sa sb) (lerp a c sa sc)]]
                      (if bb
                        [[b (lerp b c sb sc) (lerp b a sb sa)]]
                        [[c (lerp c a sc sa) (lerp c b sc sb)]]))
                    :else nil)))
         ;; body -> world orientation, then world -> the frame whose up axis
         ;; is the water plane normal (identity on level water)
         [q0 q1 q2] (plane-frame [wx wy wz])
         [[m00 m01 m02] [m10 m11 m12] [m20 m21 m22]]
         (let [r (quat-matrix quat)
               col (fn [j] [((r 0) j) ((r 1) j) ((r 2) j)])]
           (mapv (fn [q] (mapv (fn [j] (dot3 q (col j))) [0 1 2]))
                 [q0 q1 q2]))
         xf (fn [u]
              (let [ux (u 0) uy (u 1) uz (u 2)]
                [(+ (* m00 ux) (* m01 uy) (* m02 uz))
                 (- (+ (* m10 ux) (* m11 uy) (* m12 uz)) dy)
                 (+ (* m20 ux) (* m21 uy) (* m22 uz))]))
         ;; one kept triangle -> [vol6 mx my mz]; coordinates are already in
         ;; the world-oriented y=0-plane frame, so the divergence sums are the
         ;; exact ones (mesh-volume + mesh-centroid fused into one pass)
         accum (fn [[vol mx my mz] [a b c]]
                 (let [[ax ay az] (xf a) [bx by bz] (xf b) [cx cy cz] (xf c)
                       d1x (- bx ax) d1y (- by ay) d1z (- bz az)
                       d2x (- cx ax) d2y (- cy ay) d2z (- cz az)
                       wx (- (* d1y d2z) (* d1z d2y))
                       wy (- (* d1z d2x) (* d1x d2z))
                       wz (- (* d1x d2y) (* d1y d2x))
                       q (fn [a' b' c'] (+ (* a' a' 0.5)
                                           (/ (+ (* b' b') (* c' c')) 12.0)
                                           (/ (+ (* a' b') (* a' c')) 3.0)
                                           (/ (* b' c') 12.0)))]
                   [(+ vol (* wx (+ ax bx cx)))
                    (+ mx (* wx 0.5 (q ax d1x d2x)))
                    (+ my (* wy 0.5 (q ay d1y d2y)))
                    (+ mz (* wz 0.5 (q az d1z d2z)))]))
         ;; fully-dry triangles clip to nil and reduce away untouched
         [vol6 mx my mz] (reduce (fn [acc t] (reduce accum acc (clip t)))
                                 [0.0 0.0 0.0 0.0] us)
         v (/ vol6 6.0)]
     (if (< (Math/abs v) 1e-9)
       {:volume 0.0 :centroid nil}
       ;; the centroid comes back out of the plane frame (transpose of the
       ;; rotation, i.e. its rows read as columns) and then off the origin
       (let [c [(/ mx v) (+ (/ my v) dy) (/ mz v)]]
         {:volume v
          :centroid (mapv (fn [j] (+ (pos j)
                                     (* (q0 j) (c 0))
                                     (* (q1 j) (c 1))
                                     (* (q2 j) (c 2))))
                          [0 1 2])})))))

(defn buoyancy-force
  "Archimedes: rho g V displaced, straight up, applied at the centre of
  buoyancy so the offset from the centre of mass rights the ship. nil when
  nothing is submerged."
  ([body] (buoyancy-force body WATER-LEVEL))
  ([body water]
   (let [{:keys [volume centroid]} (submerged-metrics body water)]
     (when (and (pos? volume) centroid)
       {:force [0.0 (* WATER-DENSITY GRAVITY volume) 0.0]
        :point centroid
        :volume volume}))))

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

(defn hull-span
  "Half-extents [ex ey ez] of the cell set about the anchor - the hull's
  own footprint, which is where voxel.physics samples the water under it."
  [cells anchor]
  (let [ks (keys cells)]
    (if (empty? ks)
      [0.5 0.5 0.5]
      (mapv (fn [axis]
              (let [a (double (anchor axis))]
                (+ 0.5 (reduce (fn [m c]
                                 (max m (Math/abs (- (+ (c axis) 0.5) a))))
                               0.0 ks))))
            [0 1 2]))))

(defn hull-centre
  "The hull's centre of mass in anchor-relative coordinates - every cell
  weighs the same, so it is the mean cell centre.

  The anchor is a grid landmark, not a balance point: on the dreadnought it
  sits half a cell off the centreline and a couple of cells below the mass.
  Driving her from there puts a permanent couple on the hull - she steams in
  a circle with the helm amidships and noses down under power."
  [cells anchor]
  (let [ks (keys cells)
        n (double (count ks))]
    (if (zero? n)
      [0.0 0.0 0.0]
      (mapv (fn [axis]
              (- (/ (reduce (fn [a c] (+ a (c axis) 0.5)) 0.0 ks) n)
                 (anchor axis)))
            [0 1 2]))))

(defn surface-cache
  "Static per-hull data for the floatation hot path: the closed surface mesh
  as anchor-relative triangles, the exposed faces, the hull's footprint and
  its centre of mass - all recomputed only when damage changes the cells
  rather than every frame."
  [cells anchor]
  {:tris (mapv (fn [[a b c]]
                 [(mapv - a anchor) (mapv - b anchor) (mapv - c anchor)])
               (mesh/surface-triangles cells))
   :faces (surface-faces cells)
   :span (hull-span cells anchor)
   :com (hull-centre cells anchor)})

(defn openings-below
  "Count of intake openings: exposed fracture faces (not original skin) whose
  centre is below the waterline and whose normal does not point sharply up.
  A body with no recorded skin is watertight (loose rubble, debris).

  Both tests run in body-local space against the local water plane: the
  plane height at a face centre and the world normal's y component are each
  one dot product on the cached face list."
  ([body] (openings-below body WATER-LEVEL))
  ([{:keys [skin cells faces anchor] :as body} water]
    (if (nil? skin)
      0
      (let [breaches (clojure.set/difference (or faces (surface-faces cells)) skin)
            [nx ny nz d] (local-water-plane body water)
            [ax ay az] anchor]
        (count (filter (fn [[cell dir]]
                        (let [cu [(+ (- (cell 0) ax) 0.5 (* 0.5 (dir 0)))
                                  (+ (- (cell 1) ay) 0.5 (* 0.5 (dir 1)))
                                  (+ (- (cell 2) az) 0.5 (* 0.5 (dir 2)))]]
                          (and (< (+ (* nx (cu 0)) (* ny (cu 1)) (* nz (cu 2))) d)
                               (<= (+ (* nx (dir 0)) (* ny (dir 1)) (* nz (dir 2))) 0.25))))
                      breaches))))))

(defn step-flooding
  "Accumulate ingressed water for dt seconds: FLOOD-RATE per submerged
  opening, capped at one unit per cell of hull."
  ([body dt] (step-flooding body dt WATER-LEVEL))
  ([body dt water]
   (let [cap (count (:cells body))
         rate (* FLOOD-RATE (openings-below body water))]
     (assoc body :flood (min (double cap)
                             (+ (or (:flood body) 0.0) (* rate dt)))))))

(defn flood-force
  "Weight of the water taken on: rho g flood straight down, applied at the
  lowest cell centre so the ship trims bow/stern and lists toward the hole.
  nil when the hull is dry. The lowest world-y cell minimises n.u against
  the local water plane - one dot product per cell, no rotations."
  ([body] (flood-force body WATER-LEVEL))
  ([body water]
  (let [f (or (:flood body) 0.0)]
    (if (<= f 0.0)
      nil
      (let [[nx ny nz] (local-water-plane body water)
            [ax ay az] (:anchor body)
            depth (fn [c] (+ (* nx (- (+ (c 0) 0.5) ax))
                             (* ny (- (+ (c 1) 0.5) ay))
                             (* nz (- (+ (c 2) 0.5) az))))
            lowest (reduce (fn [a b] (if (< (depth b) (depth a)) b a))
                           (keys (:cells body)))
            p (cell-center->world body lowest)]
        {:force [0.0 (- (* WATER-DENSITY GRAVITY f)) 0.0]
         :point p})))))
