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


;; --- following the battle ---------------------------------------------------

(defn- on-screen
  "Where a world point on the sea plane lands in the frame, as fractions of
  the half-width and half-height. Inside the frame means both are within 1."
  [{:keys [pos target fovy]} aspect p]
  (let [[f r u] (cam/basis pos target)
        uu (mapv - u (mapv #(* (/ (u 1) (f 1)) %) f))
        dx (- (nth p 0) (target 0))
        dz (- (nth p 2) (target 2))
        det (- (* (r 0) (uu 2)) (* (r 2) (uu 0)))
        a (/ (- (* dx (uu 2)) (* dz (uu 0))) det)
        b (/ (- (* (r 0) dz) (* (r 2) dx)) det)]
    [(/ a (* 0.5 fovy aspect)) (/ b (* 0.5 fovy))]))

(def ^:private ASPECT (/ (double cam/WIDTH) cam/HEIGHT))

(deftest the-camera-holds-both-ships
  (testing "however they are placed, both sit inside the frame with room"
    (doseq [[a b] [[[0.0 0.0 -18.0] [0.0 0.0 18.0]]
                   [[-25.0 0.0 -8.0] [19.0 0.0 14.0]]
                   [[40.0 0.0 40.0] [46.0 0.0 44.0]]
                   [[-31.0 0.0 12.0] [28.0 0.0 -20.0]]
                   [[0.0 0.0 0.0] [0.0 0.0 0.0]]]]
      (let [c (cam/frame [a b] ASPECT)]
        (doseq [p [a b]]
          (let [[sx sy] (on-screen c ASPECT p)]
            (is (< (Math/abs sx) 0.95) (str "off frame horizontally: " sx))
            (is (< (Math/abs sy) 0.95) (str "off frame vertically: " sy))))))))

(deftest the-camera-slides-and-zooms-but-never-turns
  (testing "it sits over the midpoint of what it is holding"
    (let [c (cam/frame [[10.0 0.0 -4.0] [30.0 0.0 16.0]] ASPECT)]
      (is (< (Math/abs (- ((:target c) 0) 20.0)) 1e-9))
      (is (< (Math/abs (- ((:target c) 2) 6.0)) 1e-9))
      (is (= (:pos c) (mapv + (:target c) cam/OFFSET)))))
  (testing "the view direction is the same wherever it goes, which is what
            lets the ocean be a window that scrolls under it"
    (let [a (cam/frame [[0.0 0.0 0.0]] ASPECT)
          b (cam/frame [[400.0 0.0 -250.0]] ASPECT)]
      (is (every? #(< (Math/abs %) 1e-12)
                  (map - (first (cam/basis (:pos a) (:target a)))
                       (first (cam/basis (:pos b) (:target b)))))))))

(deftest the-zoom-stays-within-what-the-ocean-covers
  (testing "close in it tightens, far apart it stops rather than outrunning
            the water"
    (is (= cam/MIN-FOVY (:fovy (cam/frame [[0.0 0.0 0.0]] ASPECT)))
        "a single point needs no room at all")
    (is (= cam/MAX-FOVY
           (:fovy (cam/frame [[0.0 0.0 -300.0] [0.0 0.0 300.0]] ASPECT)))
        "and it never zooms past what the sheet was built for")
    (is (< cam/MIN-FOVY
           (:fovy (cam/frame [[0.0 0.0 -45.0] [0.0 0.0 45.0]] ASPECT))
           cam/MAX-FOVY)
        "in between it actually tracks the separation")
    (is (<= (:fovy (cam/frame [[0.0 0.0 -20.0] [0.0 0.0 20.0]] ASPECT))
            (:fovy (cam/frame [[0.0 0.0 -45.0] [0.0 0.0 45.0]] ASPECT))
            (:fovy (cam/frame [[0.0 0.0 -56.0] [0.0 0.0 56.0]] ASPECT)))
        "and never tightens as they draw apart"))
  (testing "the fleets are both in shot from the very first frame, not just
            once they have closed"
    (let [c (cam/frame [w/PLAYER-POS w/ENEMY-POS] ASPECT)]
      (is (< (:fovy c) cam/MAX-FOVY)
          (str "the starting separation needs " (:fovy c)
               " of a " cam/MAX-FOVY " limit"))
      (doseq [p [w/PLAYER-POS w/ENEMY-POS]]
        (let [[sx sy] (on-screen c ASPECT p)]
          (is (< (max (Math/abs sx) (Math/abs sy)) 0.95))))))
  (testing "the ocean sheet covers the widest frame it can reach"
    (is (>= w/SEA-EXTENT (cam/view-reach cam/MAX-FOVY))
        "no edge in shot even at full zoom-out")))
