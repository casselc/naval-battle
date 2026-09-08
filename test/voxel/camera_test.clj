(ns voxel.camera-test
  "The camera footprint the ocean sizes itself from. If this is wrong the
  sea stops short of the frame and the player sees its edge."
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.camera :as cam]
            [voxel.world :as w]))

(defn- on-plane
  "Where the pixel at normalised screen offset (a, b) in [-1,1]^2 lands on
  the sea plane, by ray-casting rather than by the closed form the camera
  namespace uses - an independent check of the same geometry."
  [fovy aspect pos target a b]
  (let [[f r u] (cam/basis pos target)
        o (mapv + pos
                (mapv #(* (* 0.5 fovy aspect a) %) r)
                (mapv #(* (* 0.5 fovy b) %) u))
        t (- (/ (o 1) (f 1)))]
    (mapv + o (mapv #(* t %) f))))

(deftest half-extent-covers-every-corner-of-the-frame
  (testing "the closed form bounds the four corners a ray-cast puts on the sea"
    (let [aspect (/ (double cam/WIDTH) cam/HEIGHT)
          e (cam/sea-half-extent cam/FOVY aspect cam/POS cam/TARGET)]
      (doseq [a [-1.0 1.0]
              b [-1.0 1.0]]
        (let [[x _ z] (on-plane cam/FOVY aspect cam/POS cam/TARGET a b)]
          (is (<= (Math/abs x) (+ e 1e-9)) (str "corner x " x " within " e))
          (is (<= (Math/abs z) (+ e 1e-9)) (str "corner z " z " within " e))))))
  (testing "and it is not wastefully loose - some corner very nearly reaches it"
    (let [aspect (/ (double cam/WIDTH) cam/HEIGHT)
          e (cam/sea-half-extent cam/FOVY aspect cam/POS cam/TARGET)
          reach (apply max
                       (for [a [-1.0 1.0] b [-1.0 1.0]
                             :let [[x _ z] (on-plane cam/FOVY aspect
                                                     cam/POS cam/TARGET a b)]]
                         (max (Math/abs x) (Math/abs z))))]
      (is (> reach (* 0.99 e)) (str "reach " reach " vs bound " e)))))

(deftest half-extent-follows-the-camera
  (testing "a wider frame needs more water"
    (let [a (cam/sea-half-extent 70.0 1.778 cam/POS cam/TARGET)
          b (cam/sea-half-extent 110.0 1.778 cam/POS cam/TARGET)]
      (is (> b a))))
  (testing "an off-centre look-at pushes the requirement out"
    (let [a (cam/sea-half-extent 70.0 1.778 cam/POS [0.0 0.0 0.0])
          b (cam/sea-half-extent 70.0 1.778 [-30.0 62.0 -90.0] [30.0 0.0 30.0])]
      (is (> b a)))))

(deftest the-ocean-runs-past-the-frame
  (testing "the simulated sheet covers everything the camera can see"
    (is (> w/SEA-EXTENT (cam/sea-half-extent))
        "sea extent exceeds the visible footprint")
    (is (>= w/SEA-EXTENT (* 1.05 (cam/sea-half-extent)))
        "with margin to spare - no edge at the corners of the frame")))
