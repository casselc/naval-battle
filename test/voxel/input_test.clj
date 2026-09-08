(ns voxel.input-test
  "Where the player is pointing. The crosshair is drawn in screen space so it
  always looks right; whether the shot goes there is entirely down to
  sea-point, which is why it needs a round trip rather than an eyeball."
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.camera :as cam]
            [voxel.input :as input]
            [voxel.render :as render]))

(def ^:private ASPECT (/ (double cam/WIDTH) cam/HEIGHT))

(defn- camera-at
  [target fovy]
  {:target target :pos (mapv + target cam/OFFSET) :fovy fovy})

(defn- aim
  [c mx my]
  (input/sea-point mx my cam/WIDTH cam/HEIGHT
                   (:pos c) (:target c) (:fovy c) render/CAMERA-ORTHO))

(defn- to-screen
  "Where a point on the sea plane appears, in pixels - the inverse of
  sea-point, worked out independently from the camera basis."
  [{:keys [pos target fovy]} p]
  (let [[f r u] (cam/basis pos target)
        d (mapv - p pos)
        dot (fn [a b] (reduce + (map * a b)))
        half-h (* 0.5 fovy)
        half-w (* half-h ASPECT)
        ndc-x (/ (dot d r) half-w)
        ndc-y (/ (dot d u) half-h)]
    [(* 0.5 (+ 1.0 ndc-x) cam/WIDTH)
     (* 0.5 (- 1.0 ndc-y) cam/HEIGHT)]))

(deftest the-aim-lands-under-the-crosshair
  (testing "every pixel round-trips back to itself"
    (doseq [c [(camera-at [0.0 0.0 0.0] cam/MIN-FOVY)
               (camera-at [0.0 0.0 0.0] cam/MAX-FOVY)
               ;; and once the battle has drifted a long way from the origin
               (camera-at [140.0 0.0 -95.0] 60.0)]
            mx [60 300 480 700 900]
            my [40 180 270 380 500]]
      (let [[sx sy] (to-screen c (aim c mx my))]
        (is (< (Math/abs (- sx mx)) 0.5)
            (str "mouse " mx "," my " came back at " sx "," sy))
        (is (< (Math/abs (- sy my)) 0.5))))))

(deftest the-centre-of-the-screen-is-what-the-camera-is-looking-at
  (doseq [t [[0.0 0.0 0.0] [30.0 0.0 -12.0] [-210.0 0.0 340.0]]]
    (let [c (camera-at t 55.0)
          [x _ z] (aim c (quot cam/WIDTH 2) (quot cam/HEIGHT 2))]
      (is (< (Math/abs (- x (t 0))) 1e-6) (str "at " t))
      (is (< (Math/abs (- z (t 2))) 1e-6)))))

(deftest the-aim-follows-the-camera
  (testing "the same pixel means the same place relative to the view, so a
            player keeps aiming where they are pointing as the fight drifts"
    (let [a (camera-at [0.0 0.0 0.0] 55.0)
          b (camera-at [300.0 0.0 -220.0] 55.0)
          pa (aim a 700 180)
          pb (aim b 700 180)]
      (is (< (Math/abs (- (- (pb 0) (pa 0)) 300.0)) 1e-6))
      (is (< (Math/abs (- (- (pb 2) (pa 2)) -220.0)) 1e-6)))))

(deftest aiming-off-the-water-still-gives-a-point-in-front
  (testing "a pixel whose ray runs past the sheet is held near the view, not
            pinned to a fixed box round the world origin"
    (let [c (camera-at [400.0 0.0 400.0] cam/MAX-FOVY)
          [x _ z] (aim c 0 0)
          reach (cam/view-reach cam/MAX-FOVY ASPECT (:pos c) (:target c))]
      (is (< (Math/abs (- x 400.0)) (* 1.3 reach)))
      (is (< (Math/abs (- z 400.0)) (* 1.3 reach))))))
