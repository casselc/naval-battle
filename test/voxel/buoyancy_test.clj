(ns voxel.buoyancy-test
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.mesh :as mesh]
            [voxel.buoyancy :as b]))

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
    (testing "rate is openings x FLOOD-RATE x dt"
      (is (near (+ 1.0 (* 2 b/FLOOD-RATE 0.5))
                (:flood (b/step-flooding bod 0.5)))))
    (testing "flooding stops at hull capacity (one unit per cell)"
      (is (near 63.0 (:flood (b/step-flooding (assoc bod :flood 62.9) 10.0))))))
  (testing "an intact hull takes on nothing"
    (let [bod (assoc (docked-ship) :skin (b/skin-faces (:cells (docked-ship))) :flood 0.0)]
      (is (zero? (:flood (b/step-flooding bod 2.0)))))))

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
  (testing "flooded water weighs rho g flood, applied at the lowest cell centre"
    (let [bod (assoc (docked-ship) :flood 3.0)
          {:keys [force point]} (b/flood-force bod)]
      (is (near-v [0.0 (- (* b/WATER-DENSITY b/GRAVITY 3.0)) 0.0] force))
      (is (near -0.5 (second point)) "lowest cell centre of the j=-1 layer"))))
