(ns voxel.buoyancy-test
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.mesh :as mesh]
            [voxel.buoyancy :as b]
            [voxel.hullc :as hullc]))

(def ^:private near (fn [x y] (< (Math/abs (- (double x) (double y))) 1e-9)))
(def ^:private near-v (fn [u v] (every? #(< (Math/abs %) 1e-9) (map - (map double u) (map double v)))))

(defn- box-cells
  [nx ny nz]
  (set (for [i (range nx) j (range ny) k (range nz)] [i j k])))

(defn- body
  "Test body: cells as :stone, anchor = mean cell centre, given pose."
  [cells pos quat]
  {:cells (zipmap cells (repeat :stone))
    :anchor (mapv #(/ (reduce + %) (double (count cells)))
                  (apply mapv vector (map #(mapv + % [0.5 0.5 0.5]) cells)))
   :pos pos
   :quat quat})

;; --- quaternion rotation ------------------------------------------------------

(deftest q-rotate-basics
  (testing "identity leaves the vector alone"
    (is (near-v [1.0 2.0 3.0] (b/q-rotate [0.0 0.0 0.0 1.0] [1.0 2.0 3.0]))))
  (testing "90 deg yaw about +y maps +x to -z"
    (let [s (Math/sin (/ Math/PI 4.0)) c (Math/cos (/ Math/PI 4.0))]
      (is (near-v [0.0 0.0 -1.0] (b/q-rotate [0.0 s 0.0 c] [1.0 0.0 0.0])))
      (is (near-v [-1.0 0.0 0.0] (b/q-rotate [0.0 s 0.0 c] [0.0 0.0 -1.0])))))
  (testing "90 deg pitch about +x maps +y to +z"
    (let [s (Math/sin (/ Math/PI 4.0)) c (Math/cos (/ Math/PI 4.0))]
      (is (near-v [0.0 0.0 1.0] (b/q-rotate [s 0.0 0.0 c] [0.0 1.0 0.0])))))
  (testing "double 90 deg yaw equals the 180 deg quaternion"
    (let [s (Math/sin (/ Math/PI 4.0)) c (Math/cos (/ Math/PI 4.0))
          q [0.0 s 0.0 c]
          v [1.0 2.0 3.0]]
      (is (near-v (b/q-rotate [0.0 1.0 0.0 0.0] v)
                  (b/q-rotate q (b/q-rotate q v)))))))

(deftest yaw-quat-and-body-point
  (testing "yaw-quat pi rotates a point 180 degrees about y around the anchor"
    (let [bod (body (box-cells 2 2 2) [10.0 0.0 -4.0] (b/yaw-quat Math/PI))
          [ax ay az] (:anchor bod)
          [wx wy wz] (b/body-point->world bod [ax ay az])]
      (is (near wx 10.0)) (is (near wy 0.0)) (is (near wz -4.0))
      ;; local +x maps to world -x through the anchor
      (let [[px py pz] (b/body-point->world bod [(+ ax 1.0) ay az])]
        (is (near px 9.0)) (is (near py 0.0)) (is (near pz -4.0))))))

;; --- plane clipping -------------------------------------------------------------

(deftest clip-below-cases
  (let [below [[1.0 -1.0 0.0] [2.0 -1.0 0.0] [1.5 -1.0 1.0]]
        above (mapv #(mapv + % [0.0 2.0 0.0]) below)]
    (testing "fully below passes through unchanged"
      (is (= [below] (b/clip-below [below] 0.0))))
    (testing "fully above is dropped"
      (is (empty? (b/clip-below [above] 0.0))))
    (testing "edge crossing produces one below-water triangle with the plane vertices"
      ;; v0 below, v1 v2 above: clip to [v0, lerp(v0,v1), lerp(v0,v2)]
      (let [v0 [0.0 -1.0 0.0] v1 [0.0 1.0 0.0] v2 [1.0 1.0 0.0]
            out (b/clip-below [[v0 v1 v2]] 0.0)]
        (is (= 1 (count out)))
        (let [[a b1 c1] (first out)]
          (is (= v0 a))
          (is (near 0.0 (second b1)))
          (is (near 0.0 (second c1)))
          (is (near 0.0 (first b1)) "lerp toward v1 stays on x=0")
          (is (near 0.5 (first c1)) "lerp toward v2 reaches x=0.5"))))
    (testing "two below, one above: a quad (two triangles), all at-or-below the plane"
      (let [v0 [0.0 -1.0 0.0] v1 [1.0 -1.0 0.0] v2 [0.0 1.0 0.0]
            out (b/clip-below [[v0 v1 v2]] 0.0)]
        (is (= 2 (count out)))
        (is (every? #(<= (second %) 1e-9) (mapcat identity out)))))))

;; --- submerged volume + centre of buoyancy --------------------------------------

(deftest submerged-half-box
  (testing "axis-aligned box, two of three layers under: exact clipped volume and CoB"
    (let [bod (body (box-cells 3 3 3) [5.0 -0.5 7.0] [0.0 0.0 0.0 1.0])
          m (b/submerged-metrics bod)]
      ;; world y spans [-2, 1]: exactly 2 layers (18 cells) below the plane
      (is (near 18.0 (:volume m)))
      (is (near-v [5.0 -1.0 7.0] (:centroid m))))))

(deftest submerged-rotated-half-cell
  (testing "a yawed single cell cut mid-face: exact volume and CoB (lerp regression)"
    (let [bod (body #{[0 0 0]} [10.0 0.0 -4.0] (b/yaw-quat (* 37.0 (/ Math/PI 180.0))))
          m (b/submerged-metrics bod)]
      (is (near 0.5 (:volume m)))
      (is (near-v [10.0 -0.25 -4.0] (:centroid m))))))

(deftest submerged-yawed-half-box
  (testing "yaw does not change the submerged fraction: half volume, CoB a quarter down"
    (let [theta (* 37.0 (/ Math/PI 180.0))
          bod (body (box-cells 3 3 3) [10.0 0.0 -4.0]
                    (b/yaw-quat theta))
          m (b/submerged-metrics bod)]
      (is (near 13.5 (:volume m)) "27 cells half-submerged")
      (is (near 10.0 (first (:centroid m))))
      (is (near -0.75 (second (:centroid m))) "world height 3, CoB a quarter below the plane")
      (is (near -4.0 (nth (:centroid m) 2))))))

(deftest submerged-pitched-half-box
  (testing "a 90-degree pitch about +x: the plane is u_z = -0.25, cutting the k=1 layer"
    ;; pitch pi/2 maps +z to -y, so world y = pos.y - u_z; the waterline sits at
    ;; u_z = -0.25, k=2 is fully under (9), k=1 is 3/4 under (6.75), and larger
    ;; u_z means deeper, so the CoB sits 0.625 below the anchor in u-space
    (let [bod (body (box-cells 3 3 3) [3.0 -0.25 5.0] (b/pitch-quat (/ Math/PI 2.0)))
          m (b/submerged-metrics bod)]
      (is (near 15.75 (:volume m)))
      (is (near-v [3.0 -0.875 5.0] (:centroid m))))))

(deftest fully-submerged-tilted-hull-identity
  (testing "fully under water: displaced volume == cell count however the body tumbles"
    (let [theta (* 31.0 (/ Math/PI 180.0))
          bod (body (box-cells 3 3 3) [3.0 -8.0 5.0] (b/pitch-quat theta))
          m (b/submerged-metrics bod)]
      (is (near 27.0 (:volume m)))
      ;; centroid of the whole body = pos (anchor is the mean cell centre)
      (is (near-v [3.0 -8.0 5.0] (:centroid m)))))
  (testing "fully above water: zero volume, nil centroid"
    (let [bod (body (box-cells 2 2 2) [0.0 50.0 0.0] [0.0 0.0 0.0 1.0])
          m (b/submerged-metrics bod)]
      (is (near 0.0 (:volume m)))
      (is (nil? (:centroid m))))))

(deftest breached-hull-still-exact
  (testing "a holed hull: clipped volume == surviving cell count when fully submerged"
    (let [cells (disj (box-cells 4 3 4) [1 1 1] [2 1 2] [1 1 2])
          bod (body cells [0.0 -20.0 0.0] (b/yaw-quat 0.9))
          m (b/submerged-metrics bod)]
      (is (near (count cells) (:volume m))))))

;; --- skin, breach openings, flooding ---------------------------------------------

(defn- docked-ship
  "4x4x4 cells at identity pose straddling the plane: j=-1 layer occupies
  [-1,0) fully below, j=0..2 above. World == local coords."
  []
  (let [cells (zipmap (for [i (range 4) j (range -1 3) k (range 4)] [i j k])
                      (repeat :hull))
        anchor [2.0 1.0 2.0]]
    {:cells cells :anchor anchor :pos anchor :quat [0.0 0.0 0.0 1.0]}))

(deftest skin-covers-intact-surface
  (testing "the intact hull floods nowhere: every surface face is skin"
    (let [bod (docked-ship)]
      (is (= (count (b/surface-faces (:cells bod)))
             (count (b/skin-faces (:cells bod))))
          "intact skin == surface")
      (is (zero? (b/openings-below bod)) "no breach openings below the waterline"))))

(deftest breach-below-water-opens-intake
  (testing "removing a submerged corner cell exposes two intake faces"
    (let [ship (docked-ship)
          breached (assoc (update ship :cells dissoc [0 -1 0])
                          :skin (b/skin-faces (:cells ship)))]
      ;; neighbours [1 -1 0] (-x face) and [0 -1 1] (-z face) gain exposed
      ;; faces below the plane; the bottom face of [0 0 0] sits exactly at
      ;; y=0 and does not count
      (is (= 2 (b/openings-below breached)))))
  (testing "a breach above the waterline lets nothing in yet"
    (let [bod (assoc (update (docked-ship) :cells dissoc [1 1 1])
                     :skin (b/skin-faces (:cells (docked-ship))))]
      (is (zero? (b/openings-below bod)))))
  (testing "no skin recorded means watertight (rubble, debris)"
    (let [bod (-> (docked-ship)
                  (update :cells dissoc [0 -1 0])
                  (dissoc :skin))]
      (is (zero? (b/openings-below bod))))))

(deftest flooding-accumulates-through-openings
  (let [bod (-> (docked-ship)
                (update :cells dissoc [0 -1 0])
                (assoc :skin (b/skin-faces (:cells (docked-ship)))
                       :flood 1.0))]
    (testing "rate is FLOOD-RATE x the intake coefficient x dt"
      (is (near (+ 1.0 (* b/FLOOD-RATE (b/intake-below bod) 0.5))
                (:flood (b/step-flooding bod 0.5)))))
    (testing "flooding stops at the hull's own volume"
      (is (near 63.0 (:flood (b/step-flooding (assoc bod :flood 62.9) 10.0))))))
  (testing "an intact hull takes on nothing"
    (let [bod (assoc (docked-ship) :skin (b/skin-faces (:cells (docked-ship))) :flood 0.0)]
      (is (zero? (:flood (b/step-flooding bod 2.0)))))))

(deftest water-comes-in-faster-through-a-deeper-hole
  (testing "the head above a hole sets the speed through it, so a scratch at
            the waterline is not as deadly as a hole under the bilge - which
            is what let a single shell put a ship down"
    (let [ship (docked-ship)
          breached (assoc (update ship :cells dissoc [0 -1 0])
                          :skin (b/skin-faces (:cells ship)))
          ;; same hull, same hole, the sea standing progressively higher
          intake (fn [w] (b/intake-below breached [0.0 1.0 0.0 w]))]
      (is (< (intake 0.0) (intake 1.0) (intake 3.0))
          "deeper under, faster in")
      (is (< (intake 0.0) (* 0.7 (intake 2.0)))
          "and the difference is not marginal")))
  (testing "a hole clear of the water lets nothing in at all"
    (let [ship (docked-ship)
          high (assoc (update ship :cells dissoc [0 1 0])
                      :skin (b/skin-faces (:cells ship)))]
      (is (zero? (b/intake-below high [0.0 1.0 0.0 0.0])))
      (is (pos? (b/intake-below high [0.0 1.0 0.0 2.5]))
          "until a wave puts it under")))
  (testing "the intake is an area, so it does not depend on how finely the
            hull happens to be cut up"
    ;; the same hull at half the voxel size: four times the faces, each a
    ;; quarter the area
    (let [coarse (assoc (update (docked-ship) :cells dissoc [0 -1 0])
                        :skin (b/skin-faces (:cells (docked-ship))))]
      (is (pos? (b/intake-below coarse)))
      (is (< (b/intake-below (assoc coarse :voxel 1.0))
             (* 1.01 (b/intake-below coarse)))))))

;; --- forces ------------------------------------------------------------------------

(deftest buoyancy-force-is-rho-g-v-at-the-cob
  (testing "force magnitude and application point"
    (let [bod (body (box-cells 3 3 3) [5.0 -0.5 7.0] [0.0 0.0 0.0 1.0])
          {:keys [force point]} (b/buoyancy-force bod)]
      (is (near-v [0.0 (* b/WATER-DENSITY b/GRAVITY 18.0) 0.0] force))
      (is (near-v [5.0 -1.0 7.0] point))))
  (testing "dry body gives no force"
    (is (nil? (b/buoyancy-force (body (box-cells 1 1 1) [0.0 9.0 0.0] [0.0 0.0 0.0 1.0]))))))

(deftest flood-weight-pushes-down-at-the-low-point
  (testing "flooded water weighs rho g flood, applied at the hull's low point"
    ;; the deepest corner of her own bounding box under this pose - which
    ;; cell is lowest changes with every wave, and finding it is a pass over
    ;; the whole hull that a fine voxel grid cannot afford per frame
    (let [ship (docked-ship)
          bod (merge ship
                     (b/surface-cache (:cells ship) (:anchor ship))
                     {:flood 3.0})
          {:keys [force point]} (b/flood-force bod)]
      (is (near-v [0.0 (- (* b/WATER-DENSITY b/GRAVITY 3.0)) 0.0] force))
      (is (near -1.0 (second point))
          "the bottom of the j=-1 layer, where the water in her has run to")))
  (testing "a dry hull carries no flood weight"
    (is (nil? (b/flood-force (docked-ship))))))


;; --- floating on a tilted water plane ----------------------------------------
;;
;; The sea is a particle field, not a flat sheet at y = 0, so buoyancy is
;; taken against the plane fitted to the water under a hull. The
;; divergence-theorem identities need the cap to lie IN that plane, so the
;; sums run in a frame whose up axis is the plane normal - volume is
;; invariant under the rotation and the centroid comes back through it.

(deftest fit-water-plane-recovers-the-sampled-surface
  (testing "samples off an exact plane fit it back"
    (let [a 0.12 b -0.07 c 0.4
          pts (for [x [-6.0 -1.0 3.0 7.0] z [-5.0 0.0 4.0]]
                [x (+ (* a x) (* b z) c) z])
          [nx ny nz d] (b/fit-water-plane pts)]
      ;; the plane is y = a x + b z + c, i.e. (-a, 1, -b).p = c up to scale
      (let [l (Math/sqrt (+ (* a a) 1.0 (* b b)))]
        (is (near (/ (- a) l) nx))
        (is (near (/ 1.0 l) ny))
        (is (near (/ (- b) l) nz))
        (is (near (/ c l) d)))))
  (testing "flat water gives the horizontal plane at its height"
    (is (near-v [0.0 1.0 0.0 -1.25]
                (b/fit-water-plane [[-3.0 -1.25 2.0] [4.0 -1.25 -1.0]
                                    [0.0 -1.25 5.0] [1.0 -1.25 -6.0]]))))
  (testing "a degenerate sample pattern falls back to horizontal"
    ;; every sample at the same (x, z): no slope is recoverable
    (is (near-v [0.0 1.0 0.0 2.0]
                (b/fit-water-plane [[1.0 2.0 3.0] [1.0 2.0 3.0]])))))

(deftest submerged-metrics-take-an-arbitrary-plane
  (testing "a horizontal plane given as a plane matches the scalar form"
    (let [bod (body (box-cells 3 3 3) [5.0 -0.5 7.0] [0.0 0.0 0.0 1.0])]
      (is (= (b/submerged-metrics bod 0.0)
             (b/submerged-metrics bod [0.0 1.0 0.0 0.0])))))
  (testing "raising the plane submerges more of the hull"
    (let [bod (body (box-cells 3 3 3) [0.0 0.0 0.0] [0.0 0.0 0.0 1.0])]
      (is (near 13.5 (:volume (b/submerged-metrics bod [0.0 1.0 0.0 0.0]))))
      (is (near 22.5 (:volume (b/submerged-metrics bod [0.0 1.0 0.0 1.0])))
          "a metre of swell puts another layer under")))
  (testing "tilting the plane is the same as tilting the hull the other way"
    ;; a level box under water sloped by theta displaces exactly what a box
    ;; rolled by theta displaces under level water
    (let [theta 0.3
          s (Math/sin theta) c (Math/cos theta)
          ;; plane normal rolled about +z, through the origin
          plane [(- s) c 0.0 0.0]
          level (body (box-cells 3 3 3) [0.0 0.0 0.0] [0.0 0.0 0.0 1.0])
          rolled (body (box-cells 3 3 3) [0.0 0.0 0.0]
                       [0.0 0.0 (Math/sin (/ theta 2.0)) (Math/cos (/ theta 2.0))])
          a (b/submerged-metrics level plane)
          bb (b/submerged-metrics rolled 0.0)]
      (is (near (:volume a) (:volume bb)))))
  (testing "a fully submerged hull displaces its cell count under any plane"
    (let [bod (body (box-cells 3 3 3) [0.0 -40.0 0.0] (b/yaw-quat 0.7))
          n (Math/sqrt 3.0)
          plane [(/ 1.0 n) (/ 1.0 n) (/ 1.0 n) 0.0]]
      (is (near 27.0 (:volume (b/submerged-metrics bod plane))))
      (is (near-v [0.0 -40.0 0.0] (:centroid (b/submerged-metrics bod plane))))))
  (testing "a hull clear above a tilted plane displaces nothing"
    (let [bod (body (box-cells 2 2 2) [0.0 60.0 0.0] [0.0 0.0 0.0 1.0])]
      (is (zero? (:volume (b/submerged-metrics bod [-0.2 0.96 0.2 0.0])))))))

(deftest buoyancy-on-a-slope-pushes-along-the-plane-normal
  (testing "uplift stays vertical, but the centre of buoyancy shifts to the
            deeper side, which is what rolls a hull on a wave"
    ;; normal (-0.24, 0.97, 0) through the origin is the surface
    ;; y = 0.247x - water piled up over +x, so that side of the hull carries
    ;; more displaced volume and the CoB moves there
    (let [bod (body (box-cells 3 3 3) [0.0 0.0 0.0] [0.0 0.0 0.0 1.0])
          flat (b/buoyancy-force bod [0.0 1.0 0.0 0.0])
          slope (b/buoyancy-force bod [-0.24 0.97 0.0 0.0])]
      (is (near 0.0 (first (:point flat))) "level water: CoB amidships")
      (is (pos? (first (:point slope)))
          "sloped water: CoB toward the submerged side")
      (is (near 0.0 (nth (:point slope) 2)) "no shift across an unsloped axis")
      (is (> (:volume (b/submerged-metrics bod [-0.24 0.97 0.0 0.0]))
             0.0))
      (is (zero? (first (:force slope))) "the force itself is still straight up")
      (is (pos? (second (:force slope)))))))

(deftest flooding-and-breaches-follow-the-same-plane
  (testing "a swell that lifts the water opens breaches that were dry"
    (let [ship (docked-ship)
          ;; knock out a cell in the j=1 layer, above the still waterline
          breached (assoc (update ship :cells dissoc [0 1 0])
                          :skin (b/skin-faces (:cells ship)))]
      (is (zero? (b/openings-below breached [0.0 1.0 0.0 0.0]))
          "still water: the hole is dry")
      (is (pos? (b/openings-below breached [0.0 1.0 0.0 2.5]))
          "a wave over the hole starts flooding her")))
  (testing "flood weight uses the plane too"
    (let [bod (assoc (docked-ship) :flood 3.0)
          {:keys [force]} (b/flood-force bod [0.0 1.0 0.0 0.0])]
      (is (near-v [0.0 (- (* b/WATER-DENSITY b/GRAVITY 3.0)) 0.0] force)))))


;; --- the shipped sums are the paper's sums -----------------------------------
;;
;; submerged-metrics fuses clipping, the volume sum and the three moment sums
;; into one allocation-free pass because it runs per body per step. That makes
;; it a second implementation of what voxel.mesh spells out straight from the
;; divergence theorem, so these hold the two together: the readable version is
;; the one that documents the algorithm, and it has to be the one that ships.

(defn- posed-tris
  "The body's closed surface mesh under its live pose, in world coordinates."
  [bod]
  (mapv (fn [t] (mapv #(b/body-point->world bod %) t))
        (mesh/surface-triangles (:cells bod))))

(deftest fused-sums-match-the-reference-transcription
  (testing "fully submerged: the closed mesh needs no clipping at all"
    (doseq [q [[0.0 0.0 0.0 1.0] (b/yaw-quat 0.9) (b/pitch-quat 0.4)]]
      (let [bod (body (box-cells 3 2 4) [2.0 -30.0 -5.0] q)
            m (b/submerged-metrics bod)
            tris (posed-tris bod)]
        (is (near (mesh/mesh-volume tris) (:volume m))
            "V = (1/6) SUM (d1 x d2)_x (x0 + x1 + x2)")
        (is (near-v (mesh/mesh-centroid tris) (:centroid m))))))
  (testing "partially submerged: clip-below then the same reference sums"
    (doseq [q [[0.0 0.0 0.0 1.0] (b/yaw-quat 0.6)]
            y [-0.4 0.0 0.7]]
      (let [bod (body (box-cells 3 3 3) [1.0 y 4.0] q)
            m (b/submerged-metrics bod)
            open (b/clip-below (posed-tris bod) b/WATER-LEVEL)]
        (is (near (mesh/mesh-volume open) (:volume m))
            "the open clipped mesh carries the closed solid's volume")
        (when (pos? (:volume m))
          (is (near-v (mesh/mesh-centroid open) (:centroid m)))))))
  (testing "a breached hull, where the mesh has interior faces too"
    (let [cells (disj (box-cells 4 3 4) [1 1 1] [2 1 2] [1 1 2] [0 0 0])
          bod (body cells [0.0 -0.2 0.0] (b/yaw-quat 0.3))
          m (b/submerged-metrics bod)
          open (b/clip-below (posed-tris bod) b/WATER-LEVEL)]
      (is (near (mesh/mesh-volume open) (:volume m)))
      (is (near-v (mesh/mesh-centroid open) (:centroid m))))))

(deftest a-unit-cell-displaces-exactly-one
  (testing "the divergence sum on the simplest possible closed mesh"
    (is (near 1.0 (mesh/mesh-volume (mesh/surface-triangles {[0 0 0] :hull}))))
    (is (near 1.0 (:volume (b/submerged-metrics
                            (body #{[0 0 0]} [0.0 -20.0 0.0]
                                  [0.0 0.0 0.0 1.0])))))))

;; --- the native solve is the same solve --------------------------------------
;;
;; The per-step floatation solve runs in C so a hull can be made of as many
;; voxels as it looks like it should be. Everything above is the reference for
;; it, and these hold the two together - if they drift, the readable version
;; stops describing what floats the ships.

(defn- native-hull
  "Load cells into the kernel and return [hull-id info]."
  [cells anchor voxel]
  [0 (hullc/set-hull! 0 (keys cells) anchor voxel)])

(deftest the-native-solve-matches-the-reference
  (doseq [voxel [1.0 0.5]]
    (testing (str "at " voxel " units per cell")
      (let [cells (zipmap (for [i (range 5) j (range 4) k (range 9)] [i j k])
                          (repeat :hull))
            anchor [2.5 0.0 4.5]
            [hull info] (native-hull cells anchor voxel)
            cache (b/surface-cache cells anchor voxel)]
        (testing "same exposed faces, footprint and balance point"
          (is (= (set (:faces info)) (:faces cache)))
          (is (near-v (:span info) (:span cache)))
          (is (near-v (:com info) (:com cache))))
        (testing "same displaced volume and centre of buoyancy, at any pose
                  and against any water plane"
          (doseq [pos [[0.0 -1.0 0.0] [4.0 -0.3 -7.0] [0.0 -30.0 0.0]
                       [0.0 30.0 0.0]]
                  quat [[0.0 0.0 0.0 1.0] (b/yaw-quat 0.9) (b/pitch-quat 0.4)]
                  plane [[0.0 1.0 0.0 0.0] [-0.2 0.96 0.18 0.5]]]
            (let [body (merge {:cells cells :anchor anchor :pos pos :quat quat}
                              cache)
                  want (b/submerged-metrics body plane)
                  got (hullc/metrics hull pos quat plane)]
              (is (< (Math/abs (- (:volume want) (:volume got))) 1e-9)
                  (str "volume at " pos " " quat " " plane))
              (when (:centroid want)
                (is (near-v (:centroid want) (:centroid got))
                    (str "centre of buoyancy at " pos))))))
        (hullc/free-hull! hull)))))

(deftest a-finer-hull-is-the-same-ship
  (testing "halving the voxel size gives eight times the cells and the same
            vessel - which is the whole point of a volume solve that is
            linear in surface triangles"
    (let [coarse (zipmap (for [i (range 4) j (range 4) k (range 8)] [i j k])
                         (repeat :hull))
          fine (zipmap (for [i (range 8) j (range 8) k (range 16)] [i j k])
                       (repeat :hull))
          a-c [2.0 0.0 4.0]
          a-f [4.0 0.0 8.0]
          pos [0.0 -1.0 0.0]
          quat (b/yaw-quat 0.6)
          plane [0.0 1.0 0.0 0.0]
          m (fn [cells anchor voxel]
              (b/submerged-metrics
               (merge {:cells cells :anchor anchor :pos pos :quat quat}
                      (b/surface-cache cells anchor voxel))
               plane))
          c (m coarse a-c 1.0)
          f (m fine a-f 0.5)]
      (is (= (* 8 (count coarse)) (count fine)))
      (is (< (Math/abs (- (:volume c) (:volume f))) 1e-9)
          (str "she displaces " (:volume c) " either way"))
      (is (near-v (:centroid c) (:centroid f))
          "and floats at the same centre of buoyancy"))))
