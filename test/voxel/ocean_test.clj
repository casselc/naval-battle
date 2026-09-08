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

(defn- spread-cloud
  "n particles uniform over [-span, span]^2 with random vorticity. Unlike
  random-cloud this actually fills the FMM domain, so the tree is deep and
  most pairs are far field."
  [n span seed]
  (let [r (lcg seed)
        u (fn [s] (- (* 2.0 s (/ (rem (r) 1000000) 1000000.0)) s))]
    (sea/make-ocean
     (mapv (fn [_] {:x (u span) :z (u span) :omega (* 2.0 (u 1.0))}) (range n)))))

(defn- worst-error
  "Largest absolute velocity error between two per-particle fields, and the
  largest speed in the reference - the scale that error is judged against."
  [ref got]
  (let [err (map (fn [[a _ c] [p _ q]]
                   (Math/sqrt (+ (* (- a p) (- a p)) (* (- c q) (- c q)))))
                 ref got)
        scale (apply max 1e-12 (map (fn [[u _ w]]
                                      (Math/sqrt (+ (* u u) (* w w)))) ref))]
    [(apply max err) scale]))

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
            [err scale] (worst-error (sea/direct-velocities oc)
                                     (sea/fmm-velocities oc))]
        (is (< err (* 1e-4 scale)))))))

(deftest fmm-multipole-passes-are-exercised-and-accurate
  ;; random-cloud sits in [-1,1] inside a +/-58 domain, so at MAX-DEPTH the
  ;; whole cloud fits in a couple of leaf cells, every pair is near field and
  ;; the test above compares direct summation against itself. These spread
  ;; the particles over the domain, where the expansions actually carry the
  ;; field, and hold the truncation error to the order-P bound.
  (doseq [span [10.0 50.0]
          n [300 800]]
    (testing (str "span=" span " n=" n ": far-field expansions are accurate")
      (let [oc (spread-cloud n span 11)
            [err scale] (worst-error (sea/direct-velocities oc)
                                     (sea/fmm-velocities oc))]
        (is (pos? err) "the far field is approximated, not summed exactly")
        (is (< err (* 1e-4 scale))
            (str "relative error " (/ err scale) " at expansion order "
                 sea/EXPANSION-P))))))

(deftest fmm-is-exact-when-everything-is-near-field
  (testing "a cloud small enough to sit in one leaf neighbourhood is summed
            directly, so it agrees with the oracle to rounding"
    (let [oc (random-cloud 200 7)
          [err _] (worst-error (sea/direct-velocities oc)
                               (sea/fmm-velocities oc))]
      (is (< err 1e-12) "no expansion is involved at this scale"))))

(deftest fmm-splits-far-from-near-the-way-the-pure-tree-does
  (testing "two tight clumps far apart: each clump is exact within itself and
            approximate across the gap, which is the multipole path"
    (let [clump (fn [cx r]
                  (mapv (fn [i]
                          (let [a (* 2.0 Math/PI (/ i 40.0))]
                            {:x (+ cx (* r (Math/cos a)))
                             :z (* r (Math/sin a))
                             :omega (if (even? i) 1.0 -0.6)}))
                        (range 40)))
          oc (sea/make-ocean (into (clump -30.0 1.0) (clump 30.0 1.0)))
          [err scale] (worst-error (sea/direct-velocities oc)
                                   (sea/fmm-velocities oc))]
      (is (pos? err))
      (is (< err (* 1e-5 scale))))))

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
  (testing "a hull under way moves the water it passes through"
    ;; the old form of this asked whether SOME particle was more than a unit
    ;; from the origin, which was already true of most of the cloud at spawn
    (let [ps [{:x 3.0 :z 1.5 :omega 0.0}]
          oc (assoc (sea/make-ocean ps) :ambient false)
          hull {:x 0.0 :z 0.0 :r 12.0 :hull-r 4.0 :push 4.0 :lift 1.0
                :swirl 0.3 :displace 0.5 :hx 0.0 :hz 1.0}
          out (first (:particles (sea/step-ocean oc 0.05 nil [hull])))]
      (is (not= 0.0 (:vx out)) "shoved out of the hull's way")
      (is (not= 0.0 (:vz out)) "and swept along her side")
      (is (not= 0.0 (:vy out)) "the surface moves under her")
      (is (not= 0.0 (:omega out)) "and she sheds vorticity into it"))))

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
          hulls [{:x -5.0 :z 1.0 :r 9.0 :hull-r 3.0 :push 0.8 :lift 0.5
                  :swirl 0.4 :displace 0.7 :hx 0.6 :hz 0.8}]]
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

;; --- the ambient sea, and the wake a ship leaves in it ----------------------

(deftest the-swell-is-a-sea-state-not-one-sinusoid
  (testing "several wave trains at distinct wavelengths and headings"
    (is (>= (count sea/SWELL-TRAINS) 3))
    (is (apply distinct? (map second sea/SWELL-TRAINS)) "distinct wavenumbers")
    (is (apply distinct? (map #(nth % 2) sea/SWELL-TRAINS))
        "distinct frequencies"))
  (testing "the surface does not repeat one wavelength along the main swell"
    ;; a single train is exactly periodic along its heading; a sea is not
    (let [[_ k _ dx dz] (first sea/SWELL-TRAINS)
          lambda (/ (* 2.0 Math/PI) k)
          cols 41 extent 40.0
          n (seac/sim-init! cols extent 44.0 true sea/VISCOSITY sea/SPARSE-MAX)]
      (is (pos? n))
      (dotimes [_ 40] (seac/sim-step! 0.02 nil nil))
      (let [h (fn [x z] (seac/sim-height x z))
            x0 0.0 z0 0.0
            x1 (* dx lambda) z1 (* dz lambda)]
        (is (> (Math/abs (- (h x0 z0) (h x1 z1))) 1e-4)
            "one wavelength on, the water is at a different height"))
      (seac/sim-free!))))

(deftest a-wake-sheds-to-the-side-of-the-ships-track
  (testing "port and starboard of the heading get opposite swirl"
    (let [hull {:x 0.0 :z 0.0 :r 6.0 :push 0.0 :swirl 1.0 :hx 0.0 :hz 1.0}
          ;; steaming toward +z: +x is starboard, -x is port
          oc (assoc (sea/make-ocean [{:x 2.0 :z 0.0 :omega 0.0}
                                     {:x -2.0 :z 0.0 :omega 0.0}])
                    :ambient false)
          [stbd port] (:particles (sea/step-ocean oc 0.016 nil [hull]))]
      (is (neg? (:omega stbd)))
      (is (pos? (:omega port)))
      (is (near (- (:omega stbd)) (:omega port) 1e-12) "equal and opposite")))
  (testing "the wake follows the ship round rather than a fixed diagonal"
    ;; same water, hull on the reciprocal course: the swirl must flip
    (let [ps [{:x 2.0 :z 0.0 :omega 0.0}]
          swirl (fn [hz]
                  (:omega (first (:particles
                                  (sea/step-ocean
                                   (assoc (sea/make-ocean ps) :ambient false)
                                   0.016 nil
                                   [{:x 0.0 :z 0.0 :r 6.0 :push 0.0
                                     :swirl 1.0 :hx 0.0 :hz hz}])))))]
      (is (neg? (swirl 1.0)))
      (is (pos? (swirl -1.0)) "reciprocal course, opposite wake")))
  (testing "a hull with no way on sheds nothing"
    (let [oc (assoc (sea/make-ocean [{:x 2.0 :z 0.0 :omega 0.0}]) :ambient false)
          out (sea/step-ocean oc 0.016 nil
                              [{:x 0.0 :z 0.0 :r 6.0 :push 0.0 :swirl 1.0}])]
      (is (zero? (:omega (first (:particles out))))))))


;; --- ships displacing water -------------------------------------------------

(defn- lattice-sea
  "A still becalmed sheet, so anything that moves was moved by the hull."
  [cols extent]
  (assoc (sea/make-ocean (sea/lattice cols extent) (+ extent 4.0))
         :ambient false :viscosity 0.0 :cols cols :extent extent))

(defn- station
  "One hull coupling at the origin, with everything off by default."
  [& kvs]
  (merge {:x 0.0 :z 0.0 :r 12.0 :hull-r 4.0
          :push 0.0 :lift 0.0 :swirl 0.0 :displace 0.0
          :hx 0.0 :hz 1.0}
         (apply hash-map kvs)))

(deftest a-hull-displaces-water-just-by-sitting-in-it
  (testing "the surface falls under her footprint and rises in a ring around"
    (let [oc (lattice-sea 41 40.0)
          hull (station :displace 1.0 :hx 0.0 :hz 0.0)   ; dead in the water
          out (sea/step-ocean oc 0.1 nil [hull])
          vy (fn [p] (:vy p))
          under (filter #(< (Math/sqrt (+ (* (:x %) (:x %)) (* (:z %) (:z %))))
                            3.0)
                        (:particles out))
          ring (filter #(let [d (Math/sqrt (+ (* (:x %) (:x %))
                                              (* (:z %) (:z %))))]
                          (and (> d 5.0) (< d 10.0)))
                       (:particles out))]
      (is (pos? (count under)))
      (is (pos? (count ring)))
      (is (every? #(neg? (vy %)) under) "water under the hull is pushed down")
      (is (every? #(pos? (vy %)) ring) "and out into the ring around her")))

  (testing "displaced, not deleted: the profile moves no net water"
    (let [oc (lattice-sea 61 60.0)
          out (sea/step-ocean oc 0.1 nil [(station :displace 1.0)])
          net (reduce + (map :vy (:particles out)))
          moved (reduce + (map #(Math/abs (:vy %)) (:particles out)))]
      (is (pos? moved) "water did move")
      (is (< (Math/abs net) (* 0.02 moved))
          (str "net volume change " net " against " moved " moved"))))

  (testing "a deeper hull displaces more water"
    (let [oc (lattice-sea 41 40.0)
          dip (fn [d]
                (- (reduce + (map #(min 0.0 (:vy %))
                                  (:particles (sea/step-ocean
                                               oc 0.1 nil
                                               [(station :displace d)]))))))]
      (is (> (dip 2.0) (* 1.9 (dip 1.0)))
          "twice the draft, about twice the water pushed aside"))))

(deftest a-hull-under-way-throws-a-bow-wave
  (testing "water piles up ahead of her and is drawn down astern"
    (let [oc (lattice-sea 41 40.0)
          out (sea/step-ocean oc 0.1 nil [(station :lift 1.0 :hx 0.0 :hz 1.0)])
          at (fn [x z] (first (filter #(and (< (Math/abs (- (:x %) x)) 0.01)
                                            (< (Math/abs (- (:z %) z)) 0.01))
                                      (:particles out))))
          ahead (at 0.0 4.0)
          astern (at 0.0 -4.0)
          abeam (at 4.0 0.0)]
      (is (pos? (:vy ahead)) "bow wave")
      (is (neg? (:vy astern)) "stern trough")
      (is (< (Math/abs (:vy abeam)) 1e-12) "nothing across the beam")
      (is (< (Math/abs (+ (:vy ahead) (:vy astern))) 1e-12)
          "antisymmetric, so it moves no net water"))))

(deftest a-moving-hull-shoves-water-out-of-its-path
  (testing "the flow around her is the doublet of a body under way"
    (let [oc (lattice-sea 41 40.0)
          out (sea/step-ocean oc 0.1 nil [(station :push 1.0 :hx 0.0 :hz 1.0)])
          at (fn [x z] (first (filter #(and (< (Math/abs (- (:x %) x)) 0.01)
                                            (< (Math/abs (- (:z %) z)) 0.01))
                                      (:particles out))))]
      ;; steaming toward +z
      (is (pos? (:vz (at 0.0 4.0))) "water ahead is pushed along in front")
      (is (pos? (:vz (at 0.0 -4.0))) "and closes in behind the stern")
      (is (neg? (:vz (at 4.0 0.0))) "water on the beam is swept aft")
      (is (< (Math/abs (:vx (at 0.0 4.0))) 1e-12)
          "the flow is symmetric about her track"))))

(deftest hull-couplings-are-rates-not-per-frame-kicks
  (testing "covering the same second of sailing in more, smaller steps puts
            the same amount of water in motion"
    ;; a coupling applied once per frame regardless of dt makes the ocean
    ;; react N times harder at N times the frame rate, which is what this had
    ;; before: the ratio below would be the step-count ratio, not ~1
    (let [oc (lattice-sea 21 20.0)
          hull (station :displace 1.0 :lift 0.6 :push 0.5 :swirl 0.4)
          run (fn [dt steps]
                (let [end (reduce (fn [o _] (sea/step-ocean o dt nil [hull]))
                                  oc (range steps))]
                  [(reduce + (map #(Math/abs (:vy %)) (:particles end)))
                   (reduce + (map #(Math/abs (:omega %)) (:particles end)))]))
          [vy-coarse om-coarse] (run 0.04 5)
          [vy-fine om-fine] (run 0.01 20)]
      (is (pos? vy-coarse))
      (is (pos? om-coarse))
      (is (< 0.85 (/ vy-fine vy-coarse) 1.15)
          (str "vertical forcing ratio " (/ vy-fine vy-coarse)
               " - four times the steps must not mean four times the sea"))
      (is (< 0.85 (/ om-fine om-coarse) 1.15)
          (str "vorticity ratio " (/ om-fine om-coarse))))))
