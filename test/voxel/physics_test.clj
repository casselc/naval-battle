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
