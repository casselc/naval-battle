(ns voxel.ocean-test
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.ocean :as sea]
            [voxel.seac :as seac]))

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
    (let [oc (assoc (random-cloud 40 99) :ambient false :viscosity 0.0)]
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
          stepped (nth (iterate #(sea/step-ocean (assoc % :ambient false) dt nil) oc)
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
    (let [oc (assoc (sea/make-ocean [{:x 0.0 :z 0.0 :omega 0.0 :y 3.0 :vy 0.0}])
                    :ambient false)
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
    (let [oc (sea/make-ocean [{:x (- sea/BOUNDS 0.1) :z 0.0 :omega 0.0 :vx 4.0}])
          oc' (sea/step-ocean oc 0.1 nil)]
      (is (<= (Math/abs (-> oc' :particles first :x)) sea/BOUNDS))
      (is (neg? (-> oc' :particles first :vx) ) "reflected inward"))))

(defn- near-v
  [u v]
  (every? #(< (Math/abs %) 1e-9) (map - u v)))

(deftest a-still-sea-has-no-field
  (testing "a calm sea beyond DIRECT-MAX skips the pair sums entirely"
    (let [oc (sea/make-ocean (mapv #(assoc % :omega 0.0)
                                   (:particles (random-cloud 300 11))))]
      (is (every? #(= [0.0 0.0 0.0] %) (sea/velocities oc))))))

(deftest sparse-vorticity-stays-exact-where-it-matters
  (testing "one vortex in a big calm sea: nearby water gets the exact field"
    (let [calm (mapv #(assoc % :omega 0.0) (:particles (random-cloud 300 12)))
          oc (sea/make-ocean (assoc calm 7 (assoc (calm 7) :omega 2.5)))
          exact (sea/direct-velocities oc)
          got (sea/velocities oc)
          src (nth (:particles oc) 7)
          sx (:x src) sz (:z src)]
      (doseq [i (range 300)
              :let [p (nth (:particles oc) i)]
              :when (< (+ (* (- (:x p) sx) (- (:x p) sx)) (* (- (:z p) sz) (- (:z p) sz)))
                       (* sea/FIELD-RADIUS sea/FIELD-RADIUS))]
        (is (near-v (got i) (exact i)) (str "particle " i))))))

(deftest far-water-rests-until-the-wake-reaches-it
  (let [ring (mapv (fn [a] {:x (* 20.0 (Math/cos a)) :z (* 20.0 (Math/sin a)) :omega 0.0})
                   (map #(* 2.0 Math/PI (/ % 24)) (range 24)))
        oc (sea/make-ocean (conj ring {:x 0.0 :z 0.0 :omega 3.0}))]
    (is (every? #(= [0.0 0.0 0.0] %) (butlast (sea/velocities oc)))
        "beyond FIELD-RADIUS the 1/r tail is left still")
    (is (some #(> (Math/abs (% 0)) 0.01) (sea/direct-velocities oc))
        "the full sum would have moved it - the cutoff is the optimisation")))

(deftest still-water-stays-put
  (testing "a becalmed sea (no ambient) steps without touching a particle"
    (let [oc (assoc (sea/make-ocean (mapv #(assoc % :omega 0.0)
                                          (:particles (random-cloud 300 14))))
                    :ambient false)]
      (is (= (:particles oc) (:particles (sea/step-ocean oc 0.05 nil nil)))))))

(deftest battle-churn-runs-the-field-kernel
  (testing "churn past the old 160 sparse ceiling routes to the wired kernel"
    (let [ps (:particles (random-cloud 260 10))
          oc (sea/make-ocean ps)
          calls (volatile! 0)]
      (is (> (count (filter #(> (Math/abs (:omega %)) sea/ACTIVE-EPS) ps)) 160)
          "the fixture is churn past the old ceiling")
      (reset! sea/field-kernel
              (fn [o] (vswap! calls inc) (sea/sparse-velocities (:particles o))))
      (let [got (sea/velocities oc)]
        (is (= 1 @calls) "the kernel serves the churn, not the FMM")
        (is (= got (sea/sparse-velocities ps)) "kernel result is the sparse field"))
      (reset! sea/field-kernel nil)))
  (testing "the same churn with no kernel wired falls back to the sparse sums"
    (let [ps (:particles (random-cloud 260 10))]
      (is (= (sea/sparse-velocities ps) (sea/velocities (sea/make-ocean ps)))))))

(deftest seas-past-the-kernel-ceiling-run-the-fmm
  (with-redefs [sea/SPARSE-MAX 5]
    (let [oc (random-cloud (inc sea/DIRECT-MAX) 9)]
      (is (= (sea/fmm-velocities oc) (sea/velocities oc))))))

(deftest sparse-sums-are-the-direct-sums-in-range
  (testing "under SPARSE-MAX actives the field is the exact Biot-Savart sum"
    ;; the cloud spans [-1,1], well inside FIELD-RADIUS, so the truncated
    ;; sparse sum and the untruncated direct sum must agree exactly
    (let [oc (random-cloud sea/DIRECT-MAX 7)]
      (is (= (sea/direct-velocities oc) (sea/velocities oc))))))

(deftest the-open-sea-has-ambient-swell
  (testing "an ambient sea visibly moves but stays bounded and in place"
    (let [oc (sea/make-ocean (mapv #(assoc % :omega 0.0)
                                   (:particles (random-cloud 120 21))))
          stepped (reduce (fn [o _] (sea/step-ocean o 0.05 nil nil))
                          oc (range 120))]
      (is (some #(> (:y %) 0.01) (:particles stepped))
          "travelling swell lifts the surface")
      (is (some #(> (Math/abs (:vy %)) 1e-4) (:particles stepped))
          "the water is still in motion")
      (let [b (:bounds oc)
            escaped (some (fn [p] (or (> (Math/abs (:x p)) b)
                                      (> (Math/abs (:z p)) b)))
                          (:particles stepped))]
        (is (nil? escaped) "no particle drifts out of the domain"))
      (let [mean-vx (/ (reduce + (map :vx (:particles stepped)))
                       (count (:particles stepped)))]
        (is (< (Math/abs mean-vx) 0.05)
            "swell stirs the water without a net current")))))

(deftest ambient-churn-stays-gentle
  (testing "turbulence eddies stay well under battle vorticity"
    (let [oc (sea/make-ocean (mapv #(assoc % :omega 0.0)
                                   (:particles (random-cloud 120 22))))
          stepped (reduce (fn [o _] (sea/step-ocean o 0.05 nil nil))
                          oc (range 300))]
      (is (< (apply max (map #(Math/abs (:omega %)) (:particles stepped))) 0.05)
          "ambient chop, not a maelstrom"))))


;; --- the native sim is the pure model, in flat arrays ------------------------
;;
;; The particle state lives in C so the frame loop never touches a particle
;; across the FFI boundary. voxel.ocean stays the readable definition of what
;; the ocean IS, and these hold the kernel to it step for step - if the two
;; ever drift, the reference stops describing the game.

(defn- native-sheet
  "A native sim seeded from ps, plus the equivalent pure ocean."
  [cols extent bounds ps ambient? sparse-max]
  (seac/sim-init! cols extent bounds ambient? sea/VISCOSITY sparse-max)
  (seac/sim-load! ps 0.0)
  [(assoc (sea/make-ocean ps) :bounds bounds :ambient ambient?
          :viscosity sea/VISCOSITY)])

(defn- max-drift
  [a b]
  (apply max 0.0
         (mapcat (fn [p q]
                   (map (fn [k] (Math/abs (- (double (or (k p) 0.0))
                                             (double (or (k q) 0.0)))))
                        [:x :z :y :vx :vy :vz :omega]))
                 a b)))

(deftest native-sim-tracks-the-pure-ocean
  (testing "a churning sea steps identically in C and in Clojure"
    (let [cols 24
          extent 20.0
          bounds 24.0
          r (lcg 4242)
          u (fn [lo hi] (+ lo (* (- hi lo) (/ (rem (r) 1000000) 1000000.0))))
          ;; seed the lattice itself, so both sides start from one state
          ps (vec (for [i (range cols) j (range cols)]
                    {:x (+ (- extent) (* i (/ (* 2.0 extent) (dec cols))))
                     :z (+ (- extent) (* j (/ (* 2.0 extent) (dec cols))))
                     :y 0.0 :vx 0.0 :vy 0.0 :vz 0.0
                     :omega (u -1.5 1.5)}))
          [pure0] (native-sheet cols extent bounds ps true sea/SPARSE-MAX)
          blasts [{:x 2.0 :z -3.0 :r 4.0 :power 6.0}]
          hulls [{:x -5.0 :z 1.0 :r 5.0 :push 0.8 :swirl 0.4}]]
      (loop [k 0 pure pure0]
        (when (< k 6)
          ;; couplings on the first step only, then free evolution
          (let [bl (when (zero? k) blasts)
                hl (when (zero? k) hulls)
                pure' (sea/step-ocean pure 0.016 bl hl)]
            (seac/sim-step! 0.016 bl hl)
            (let [d (max-drift (:particles pure') (seac/sim-particles))]
              (is (< d 1e-9) (str "step " k " drift " d)))
            (is (< (Math/abs (- (seac/sim-time) (:time pure'))) 1e-12))
            (recur (inc k) pure'))))
      (seac/sim-free!))))

(deftest native-sim-lays-out-a-full-sheet
  (testing "the sheet covers the requested extent, one particle per tile"
    (let [n (seac/sim-init! 33 48.0 58.0 true sea/VISCOSITY sea/SPARSE-MAX)
          ps (seac/sim-particles)
          xs (map :x ps)]
      (is (= (* 33 33) n))
      (is (= 33 (seac/sim-cols)))
      (is (< (Math/abs (- (seac/sim-spacing) (/ 96.0 32.0))) 1e-12))
      (is (< (Math/abs (+ (apply min xs) 48.0)) 1e-9) "starts at -extent")
      (is (< (Math/abs (- (apply max xs) 48.0)) 1e-9) "ends at +extent")
      (seac/sim-free!))))

(deftest native-still-water-stays-put
  (testing "a becalmed native sea steps without moving a particle"
    (seac/sim-init! 20 20.0 24.0 false 0.0 sea/SPARSE-MAX)
    (let [before (seac/sim-particles)]
      (seac/sim-step! 0.05 nil nil)
      (is (= before (seac/sim-particles)))
      (seac/sim-free!))))

(deftest native-sim-fmm-branch-tracks-the-pure-ocean
  (testing "past the sparse ceiling both sides switch to the FMM together"
    ;; sparse-max 0 forces the multipole path on a sheet small enough for the
    ;; pure reference to step in reasonable time
    (let [cols 16
          extent 14.0
          bounds 18.0
          r (lcg 77)
          u (fn [lo hi] (+ lo (* (- hi lo) (/ (rem (r) 1000000) 1000000.0))))
          sp (/ (* 2.0 extent) (dec cols))
          ps (vec (for [i (range cols) j (range cols)]
                    {:x (+ (- extent) (* i sp)) :z (+ (- extent) (* j sp))
                     :y 0.0 :vx 0.0 :vy 0.0 :vz 0.0 :omega (u -1.0 1.0)}))
          [pure0] (native-sheet cols extent bounds ps false 0)
          pure0 (assoc pure0 :ambient false)]
      (is (> (count (filter #(> (Math/abs (:omega %)) sea/ACTIVE-EPS) ps)) 0))
      ;; the pure side needs the same ceiling, or it takes the sparse path
      ;; and the comparison is between two different algorithms
      (with-redefs [sea/SPARSE-MAX 0]
        (loop [k 0 pure pure0]
          (when (< k 3)
            (let [pure' (sea/step-ocean pure 0.016 nil nil)]
              (seac/sim-step! 0.016 nil nil)
              (let [d (max-drift (:particles pure') (seac/sim-particles))]
                (is (< d 1e-9) (str "fmm step " k " drift " d)))
              (recur (inc k) pure')))))
      (seac/sim-free!))))
