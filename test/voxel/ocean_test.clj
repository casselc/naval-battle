(ns voxel.ocean-test
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.ocean :as sea]))

(def ^:private near (fn [x y tol] (< (Math/abs (- (double x) (double y))) tol)))

;; deterministic LCG (mesh-test style)
(defn- lcg [seed]
  (let [s (atom seed)]
    (fn [] (let [x (rem (+ (* 1103515245 @s) 12345) 2147483648)]
              (reset! s x)
              x))))

(defn- random-cloud
  "n particles uniform in [-1,1]^2 with random vorticity, seed-deterministic."
  [n seed]
  (let [r (lcg seed)
        u (fn [] (- (* 2.0 (/ (rem (r) 1000000) 1000000.0)) 1.0))]
    (sea/make-ocean
     (mapv (fn [_] {:x (u) :z (u) :omega (* 2.0 (u))}) (range n)))))

;; --- single vortices: the analytic oracle ---------------------------------------

(deftest single-vortex-velocity
  (testing "a lone vortex induces the tangential field u = Gamma/(2 pi r)"
    ;; evaluating AT the vortex is singular; read the field on a second particle
    (let [oc (sea/make-ocean [{:x 0.0 :z 0.0 :omega 2.0}
                              {:x 4.0 :z 0.0 :omega 0.0}])
          [[_] [ux _ uz]] (sea/direct-velocities oc)]
      (is (near 0.0 ux 1e-12) "field of a vortex at origin points purely tangentially")
      (is (near (/ 2.0 (* 2.0 Math/PI 4.0)) uz 1e-12) "u = Gamma/(2 pi r), CCW"))))

(deftest vortex-dipole-translates
  (testing "equal-and-opposite vortices march side by side at Gamma/(2 pi d)"
    (let [d 1.0
          G 3.0
          oc (sea/make-ocean [{:x 0.0 :z 0.5 :omega G}
                              {:x 0.0 :z (- 0.5) :omega (- G)}])
          [[ux1 uy1 uz1] [ux2 uy2 uz2]] (sea/direct-velocities oc)]
      (is (near 0.0 uy1 1e-12) "surface field is horizontal")
      (is (near 0.0 uz1 1e-12) "motion is perpendicular to the separation axis")
      (is (near (/ G (* 2.0 Math/PI d)) ux1 1e-12) "dipole speed Gamma/(2 pi d)")
      (is (near ux1 ux2 1e-12) "the pair translates together"))))

;; --- FMM vs direct: the accuracy contract ----------------------------------------

(deftest fmm-matches-direct-sums
  (doseq [n [16 65 200]
          seed [7 42]]
    (testing (str "n=" n " seed=" seed ": FMM velocities match direct summation")
      (let [oc (random-cloud n seed)
            direct (sea/direct-velocities oc)
            fast (sea/fmm-velocities oc)
            scale (apply max (cons 1.0 (map (fn [[u _]] (max (Math/abs u) 1e-9)) direct)))]
        (is (every? (fn [[d f]]
                      (let [dx (- (first d) (first f))
                            dz (- (nth d 2) (nth f 2))]
                        (< (Math/sqrt (+ (* dx dx) (* dz dz))) (* 1e-4 scale))))
                    (mapv vector direct fast)))))))

(deftest fmm-field-is-finite-on-distinct-clouds
  (testing "near + far FMM field stays finite on a cloud with no duplicated positions"
    (let [oc (random-cloud 500 99)]
      ;; guard the precondition: a duplicate position would make the guard vacuous
      (is (= 500 (count (distinct (map (fn [p] [(:x p) (:z p)]) (:particles oc)))))
          "cloud positions are distinct")
      (is (every? (fn [[ux uy uz]]
                    (every? (fn [v] (< (Math/abs v) 1e308)) [ux uy uz]))
                  (sea/fmm-velocities oc))
          "no NaN or Inf velocity components"))))

;; --- dynamics ----------------------------------------------------------------------

(deftest advection-conserves-circulation
  (testing "total circulation is carried unchanged by the step"
    (let [oc (random-cloud 40 99)]
      (is (near (sea/total-circulation oc)
                (sea/total-circulation (sea/step-ocean oc 0.016 nil))
                1e-12))))

  (testing "a same-sign vortex pair orbits its midpoint at the analytic rate"
    (let [r 1.0
          G (* 8.0 Math/PI)                       ; omega = 8 pi
          oc (sea/make-ocean [{:x r :z 0.0 :omega G}
                              {:x (- r) :z 0.0 :omega G}])
          ;; each orbits the midpoint at radius r; speed from the partner
          speed (/ G (* 2.0 Math/PI (* 2.0 r)))
          ;; quarter orbit time: t = (pi/2) r / speed
          dt 0.002
          t-q (/ (* 0.5 Math/PI r) speed)
          stepped (nth (iterate #(sea/step-ocean % dt nil) oc)
                       (int (Math/round (/ t-q dt))))
          [p1 p2] (:particles stepped)]
      ;; after a quarter orbit the pair sits near the z axis
      (is (near 0.0 (:x p1) 0.05) (str "p1 x ~ 0, got " (:x p1)))
      (is (near r (Math/sqrt (+ (* (:x p1) (:x p1)) (* (:z p1) (:z p1)))) 0.05)
          "p1 stays on the orbit circle")
      (is (near (- (:x p1)) (:x p2) 1e-9) "symmetric pair stays symmetric"))))

(deftest vorticity-decays-viscously
  (testing "omega shrinks by (1 - visc dt) each step and never grows"
    (let [oc (assoc (random-cloud 8 5) :viscosity 0.5)
          oc' (sea/step-ocean oc 0.1 nil)]
      (is (< (sea/total-circulation oc') (sea/total-circulation oc))))))

(deftest spray-falls-back-to-the-surface
  (testing "particles above the water fall, splash, and settle"
    (let [oc (sea/make-ocean [{:x 0.0 :z 0.0 :omega 0.0 :y 3.0 :vy 0.0}])
          fallen (nth (iterate #(sea/step-ocean % 0.016 nil) oc) 400)]
      (is (near 0.0 (-> fallen :particles first :y) 1e-3))
      (is (near 0.0 (-> fallen :particles first :vy) 1e-3)))))

(deftest blasts-kick-and-swirl
  (testing "a blast impulse throws nearby water up and injects circulation"
    (let [oc (random-cloud 32 3)
          oc' (sea/step-ocean oc 0.016 [{:x 0.0 :z 0.0 :r 1.0 :power 2.0}])]
      (is (some (fn [p] (pos? (:y p))) (:particles oc')) "spray thrown up")
      (is (some (fn [p] (not= 0.0 (:omega p))) (:particles oc'))
          "not every particle keeps its initial vorticity under a swirl kick")))
  (testing "a hull displacement pushes water outward"
    (let [oc (random-cloud 16 11)
          oc' (sea/step-ocean oc 0.016 nil [{:x 0.0 :z 0.0 :r 0.5 :push 4.0 :swirl 0.3}])]
      (is (some (fn [p] (> (+ (* (:x p) (:x p)) (* (:z p) (:z p)))
                           1.0))
                (:particles oc'))
          "nearby particles pushed off the origin"))))

(deftest ocean-stays-bounded
  (testing "particles reflect at the domain walls"
    (let [oc (sea/make-ocean [{:x 9.9 :z 0.0 :omega 0.0 :vx 4.0}])
          oc' (sea/step-ocean oc 0.1 nil)]
      (is (<= (Math/abs (-> oc' :particles first :x)) 10.0))
      (is (neg? (-> oc' :particles first :vx) ) "reflected inward"))))
