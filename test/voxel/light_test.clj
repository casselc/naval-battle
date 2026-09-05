(ns voxel.light-test
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.light :as light]))

(def ^:private near (fn [x y] (< (Math/abs (- (double x) (double y))) 1e-9)))
(def ^:private near-v (fn [u v] (every? #(< (Math/abs %) 1e-9) (map - (map double u) (map double v)))))

;; --- the shared-corner height field -------------------------------------------------

(deftest corners-average-adjacent-tiles
  (testing "a lone flat tile lifts all four of its corners to its height"
    (is (= {:h 1.0 :foam 0.0}
           (get (light/corner-grid {[0 0] {:h 1.0 :foam 0.0}}) [1 1]))))
  (testing "adjacent tiles share a corner at their mean, so the sheet is continuous"
    (let [g (light/corner-grid {[0 0] {:h 0.0 :foam 0.0} [1 0] {:h 2.0 :foam 1.0}})]
      (is (near 1.0 (:h (get g [1 0]) -999.0)) "shared corner is the average")
      (is (near 0.5 (:foam (get g [1 0]) -999.0)))
      (is (nil? (get g [2 2])) "corners exist only where a tile touches"))))

(deftest corner-normals-follow-the-slope
  (let [g {[0 0] {:h 0.0} [1 0] {:h 0.2} [2 0] {:h 0.4}}]
    (testing "flat water faces straight up"
      (is (near-v [0.0 1.0 0.0] (light/corner-normal {[0 0] {:h 1.0} [1 0] {:h 1.0}} 0 0 2.0))))
    (testing "a ramp rising with x tips the normal toward -x"
      (let [[nx ny nz] (light/corner-normal g 1 0 2.0)]
        (is (neg? nx))
        (is (near (/ -0.1 (Math/sqrt 1.01)) nx))
        (is (near (/ 1.0 (Math/sqrt 1.01)) ny))))))

;; --- sun shading ---------------------------------------------------------------------

(def SUN (let [x -0.45 y 0.78 z 0.35 l (Math/sqrt (+ (* x x) (* y y) (* z z)))]
           [(/ x l) (/ y l) (/ z l)]))

(deftest sun-shade-floors-at-ambient
  (testing "facing the sun is full bright, away from it is the ambient floor"
    (is (near 1.0 (light/sun-shade SUN SUN)))
    (is (near light/AMBIENT (light/sun-shade (mapv - SUN) SUN)))
    (is (near light/AMBIENT (light/sun-shade [0.0 -1.0 0.0] SUN))))
  (testing "brightness never leaves [AMBIENT, 1]"
    (is (near 1.0 (light/sun-shade [0.0 1.0 0.0] [0.0 1.0 0.0])))
    (is (> (light/sun-shade [0.0 1.0 0.0] SUN) light/AMBIENT))))

;; --- shadow projection -----------------------------------------------------------------

(deftest points-cast-onto-the-field-along-the-sun-rays
  (testing "an overhead sun drops the point straight down onto the field"
    (is (near-v [3.0 0.43 7.0]
                (light/project-to-field [3.0 5.0 7.0] (fn [_ _] 0.4) [0.0 1.0 0.0]))))
  (testing "a slanted sun lands away from the source, down-sun"
    (let [sun (let [x 0.6 y 0.8 z 0.0 l (Math/sqrt (+ (* x x) (* y y)))]
                [(/ x l) (/ y l) (/ z l)])]
      (is (near-v [-0.75 0.13 7.0]
                  (light/project-to-field [3.0 5.0 7.0] (fn [_ _] 0.1) sun))))))

(deftest batched-corner-normals-match
  (let [g {[0 0] {:h 0.1} [1 0] {:h 0.3} [0 1] {:h -0.2} [1 1] {:h 0.05}}]
    (is (= (light/corner-normal g 0 0 2.0) (get (light/corner-normals g 2.0) [0 0])))
    (is (= (light/corner-normal g 1 1 2.0) (get (light/corner-normals g 2.0) [1 1])))))
