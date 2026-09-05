(ns voxel.physics-test
  "The ocean world end to end: no ground (things sink out of sight), buoyancy
  floats voxel bodies at their draft, breaches flood and sink, and every
  floating body is driven independently (multi-shell crews of bodies)."
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.physics :as phys]
            [voxel.ship :as ship]
            [voxel.world :as w]
            [voxel.buoyancy :as buoy]))

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
