(ns voxel.physics-test
  "The ocean world end to end: no ground (things sink out of sight), buoyancy
  floats voxel bodies at their draft, breaches flood and sink, and every
  floating body is driven independently (multi-shell crews of bodies)."
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.physics :as phys]
            [voxel.ship :as ship]
            [voxel.world :as w]
            [voxel.buoyancy :as buoy]
            [voxel.ocean :as ocean]
            [voxel.seac :as seac]))

(def ^:private raft
  "2x1x3 cell hull section."
  #{[0 0 0] [1 0 0] [0 0 1] [1 0 1] [0 0 2] [1 0 2]})

(defn- settle
  "Step physics total seconds in dt chunks, returning the final facts map."
  [total dt]
  (loop [t 0.0 facts nil]
    (if (>= t total)
      facts
      (recur (+ t dt) (phys/step! dt)))))

(defn- body-y [facts id]
  (-> (filter #(= id (:body %)) (:bodies facts))
      first
      (get-in [:pos 1])))

(defn- lcg
  [seed]
  (let [s (atom seed)]
    (fn []
      (let [x (rem (+ (* 1103515245 @s) 12345) 2147483648)]
        (reset! s x)
        x))))

(defn- cloud
  "n particles over the arena with `na` seeded vortices, deterministic."
  [n na seed]
  (let [r (lcg seed)
        u (fn [] (- (* 88.0 (/ (rem (r) 1000000) 1000000.0)) 44.0))]
    (mapv (fn [i] {:x (u) :z (u) :omega (if (< i na) (- (* 3.0 (u)) 1.5) 0.0)})
          (range n))))

(deftest c-sea-field-matches-the-pure-sum
  (testing "the C kernel returns the reference sparse field"
    (let [ps (cloud 400 25 7)
          pure (ocean/velocities {:particles ps})
          c (seac/field ps ocean/FIELD-RADIUS ocean/ACTIVE-EPS)]
      (is (= (count pure) (count c)))
      (is (every? (fn [[u v]] (< (Math/abs (- (nth u 0) (nth v 0))) 1e-9))
                  (mapv vector pure c))
          "x components agree")
      (is (every? (fn [[u v]] (< (Math/abs (- (nth u 2) (nth v 2))) 1e-9))
                  (mapv vector pure c))
          "z components agree"))))

(deftest ocean-world-has-no-ground
  (phys/init!)
  (phys/spawn-ball! [0.0 3.0 0.0] [0.0 0.0 0.0])
  (let [facts (settle 5.0 0.05)]
    (is (< (get-in facts [:ball :pos 1]) -5.0)
        "the ball sinks through the waterline and keeps going - no floor")))

(deftest buoyancy-floats-voxel-bodies-at-draft
  (phys/init!)
  (let [id (phys/spawn-body! [0.0 -0.4 0.0] [0.0 0.0 0.0 1.0] 1 [0 0 0] raft)]
    (let [facts (settle 6.0 0.05)
          y (body-y facts id)]
      (is (> y -1.5) "does not sink away")
      (is (< y 1.0) "does not fly out of the water"))))

(deftest damaged-hulls-flood-and-sink
  (phys/init!)
  (let [id (phys/spawn-body! [0.0 -0.4 0.0] [0.0 0.0 0.0 1.0] 1 [0 0 0] raft)]
    ;; knock out one bottom cell: the exposed fracture faces below the
    ;; waterline are intakes, water mass accumulates and the wreck goes down
    (phys/damage-cells! id #{[0 0 1]})
    (let [facts (settle 12.0 0.05)]
      (is (< (body-y facts id) -3.0) "breached hull sinks"))))

(deftest every-floating-body-is-driven-independently
  (phys/init!)
  (let [a (phys/spawn-body! [0.0 -0.4 0.0] [0.0 0.0 0.0 1.0] 1 [0 0 0] raft)
        b (phys/spawn-body! [10.0 -0.4 0.0] [0.0 0.0 0.0 1.0] 1 [0 0 0] raft)]
    (let [facts (settle 6.0 0.05)
          ya (body-y facts a)
          yb (body-y facts b)]
      (is (and ya yb) "both bodies reported")
      (is (< (Math/abs (- ya yb)) 0.1) "twin hulls ride at the same waterline"))))

(deftest warships-ride-at-their-waterline
  (phys/init!)
  (let [layout (ship/dreadnought)
        id (phys/spawn-body! [0.0 -3.0 0.0] (buoy/yaw-quat 0.0) 1
                             (:anchor layout) (keys (:cells layout)))
        y1 (body-y (settle 10.0 0.05) id)
        y2 (body-y (settle 10.0 0.05) id)]
    (is (< y1 -2.6) "three of four hull levels are under - a warship's draft")
    (is (> y1 -5.0) "the top hull level and deck ride dry, well above SUNK-DEPTH")
    (is (< (Math/abs (- y2 y1)) 0.5)
        "holds its waterline - no foundering without a breach")))

;; --- helm: arrow-key steering ------------------------------------------------------

(defn- sail
  "Step physics total seconds, steering the body every step. Returns the
  final facts map."
  [id total dt thrust turn]
  (loop [t 0.0 facts nil]
    (if (>= t total)
      facts
      (do (phys/steer! id thrust turn)
          (recur (+ t dt) (phys/step! dt))))))

(defn- body-fact
  [facts id]
  (first (filter #(= id (:body %)) (:bodies facts))))

(defn- yaw-of
  "Heading about +y from the body quaternion."
  [fact]
  (let [[_ qy _ qw] (:quat fact)]
    (* 2.0 (Math/atan2 qy qw))))

(deftest helm-drives-the-ship-along-its-bow
  (phys/init!)
  (let [layout (ship/dreadnought)
        id (phys/spawn-body! [0.0 -3.0 0.0] (buoy/yaw-quat 0.0) 1
                             (:anchor layout) (keys (:cells layout)))
        z0 (get-in (body-fact (settle 8.0 0.05) id) [:pos 2])
        f (body-fact (sail id 5.0 0.05 1.0 0.0) id)]
    (is (> (- (get-in f [:pos 2]) z0) 4.0)
        "full ahead carries the ship several units along its bow axis")
    (is (< (Math/abs (get-in f [:pos 0])) 4.0)
        "no helm, no crab (a little trim from the aft-heavy CoM is fine)")
    (is (< -5.0 (get-in f [:pos 1]) -1.0) "still riding its waterline underway")))

(deftest helm-turns-the-ship-to-starboard
  (phys/init!)
  (let [layout (ship/dreadnought)
        id (phys/spawn-body! [0.0 -3.0 0.0] (buoy/yaw-quat 0.0) 1
                             (:anchor layout) (keys (:cells layout)))]
    (settle 8.0 0.05)
    (is (> (yaw-of (body-fact (sail id 3.0 0.05 0.0 1.0) id)) 0.15)
        "right helm swings the bow toward +x (starboard)")))

(deftest c-fmm-matches-the-pure-fmm
  (testing "the C multi-level quadtree FMM replicates the pure reference"
    (let [ps (cloud 420 80 31)
          oc (ocean/make-ocean ps)
          pure (ocean/fmm-velocities oc)
          got (seac/fmm ps 10 ocean/BOUNDS)]
      (is (= (count pure) (count got)))
      (is (every? (fn [[u v]] (< (max (Math/abs (- (nth u 0) (nth v 0)))
                                      (Math/abs (- (nth u 2) (nth v 2))))
                                 1e-8))
                  (mapv vector pure got))
          "velocities agree with the pure FMM to rounding"))))


;; --- floating on the particle surface ---------------------------------------
;;
;; Buoyancy is taken against the plane fitted to the water under each hull,
;; so these drive phys/step! with a known surface and check the hull does
;; what a ship on that water would do.

(defn- settle-on
  "Step physics on a given water surface, returning the final facts."
  [total dt water]
  (loop [t 0.0 facts nil]
    (if (>= t total)
      facts
      (recur (+ t dt) (phys/step! dt water)))))

(defn- roll-of
  "Roll angle about the +z (keel) axis, from the body quaternion."
  [fact]
  (let [[qx qy qz qw] (:quat fact)]
    (Math/atan2 (* 2.0 (+ (* qx qy) (* qz qw)))
                (- 1.0 (* 2.0 (+ (* qy qy) (* qz qz)))))))

(deftest hulls-ride-the-height-of-the-water
  (testing "water standing higher floats the hull higher"
    (phys/init!)
    (let [id (phys/spawn-body! [0.0 -0.4 0.0] [0.0 0.0 0.0 1.0] 1 [0 0 0] raft)
          low (body-y (settle-on 6.0 0.05 (fn [_ _] 0.0)) id)]
      (phys/init!)
      (let [id2 (phys/spawn-body! [0.0 -0.4 0.0] [0.0 0.0 0.0 1.0] 1 [0 0 0] raft)
            high (body-y (settle-on 6.0 0.05 (fn [_ _] 1.5)) id2)]
        (is (> high (+ low 1.0))
            (str "a metre and a half of swell lifts her: " low " -> " high))))))

(deftest hulls-heel-to-the-slope-of-the-wave
  (testing "water sloping across the beam rolls the hull"
    (phys/init!)
    (let [id (phys/spawn-body! [0.0 -0.4 0.0] [0.0 0.0 0.0 1.0] 1 [0 0 0] raft)
          ;; surface climbing toward +x across the raft's beam
          f (settle-on 4.0 0.02 (fn [x _] (* 0.35 x)))
          fact (first (filter #(= id (:body %)) (:bodies f)))]
      (is (> (Math/abs (roll-of fact)) 0.05)
          (str "she takes a list on a sloped sea, got " (roll-of fact)))))
  (testing "level water leaves her upright"
    (phys/init!)
    (let [id (phys/spawn-body! [0.0 -0.4 0.0] [0.0 0.0 0.0 1.0] 1 [0 0 0] raft)
          f (settle-on 4.0 0.02 (fn [_ _] 0.0))
          fact (first (filter #(= id (:body %)) (:bodies f)))]
      (is (< (Math/abs (roll-of fact)) 0.02)
          (str "no slope, no list, got " (roll-of fact))))))

(deftest the-live-ocean-is-what-the-hulls-float-on
  (testing "the surface sampler reads the simulated particles, not y = 0"
    (phys/init!)
    (let [oc (:ocean (w/initial-state))
          water (ocean/surface-fn oc)]
      (is (:native oc) "the game ocean runs on the native sim")
      ;; a blast throws water up; the sampler must see it under that spot
      (let [before (water 6.0 -4.0)
            oc' (ocean/step-ocean oc 0.016 [{:x 6.0 :z -4.0 :r 8.0 :power 12.0}] nil)
            after ((ocean/surface-fn oc') 6.0 -4.0)]
        (is (> after (+ before 0.05))
            (str "the shell splash shows in the water the hulls float on: "
                 before " -> " after))))))

(deftest pure-and-native-surface-sampling-agree
  (testing "the Clojure surface reader matches the C one on the same lattice"
    (let [cols 21 extent 20.0
          n (seac/sim-init! cols extent 24.0 true 0.04 2048)]
      (seac/sim-step! 0.05 [{:x 3.0 :z -2.0 :r 9.0 :power 5.0}] nil)
      (let [ps (seac/sim-particles)
            pure {:particles ps :cols cols :extent extent}]
        (is (= (* cols cols) n))
        (doseq [x [-19.3 -7.0 0.4 11.2 19.9 -40.0 40.0]
                z [-18.1 -3.3 0.0 8.8 19.5]]
          (is (< (Math/abs (- (seac/sim-height x z)
                              (ocean/surface-height pure x z)))
                 1e-12)
              (str "sample " x " " z))))
      (seac/sim-free!))))
