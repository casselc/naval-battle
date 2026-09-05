(ns voxel.world-test
  "The naval battle as pure state: two fleets, ballistic shells that carve
  hulls, an enemy that returns fire, a sea that feels every blast, and a
  winner when one ship goes under."
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.world :as w]
            [voxel.buoyancy :as buoy]))

(defn- dist
  [a b]
  (Math/sqrt (reduce + (map #(* % %) (map - a b)))))

(defn- rigged
  "Initial state with both fleets attached to fabricated physics bodies, so
  physics facts fold by body id."
  []
  (-> (w/initial-state)
      (assoc-in [:ships :player :body] 101)
      (assoc-in [:ships :enemy :body] 102)))

(defn- facts-for
  "Static physics facts echoing each ship's own pose (nothing is moving)."
  [st]
  {:bodies (mapv (fn [s] {:body (:body s) :pos (:pos s) :quat (:quat s)
                          :vel [0.0 0.0 0.0] :speed 0.0 :asleep false})
                 (vals (:ships st)))})

(defn- cruise
  "Step the battle n frames of dt under static facts; returns every
  intermediate state."
  [st n dt]
  (loop [i 0 st st hist []]
    (if (= i n)
      hist
      (let [st' (w/step-state st dt (facts-for st))]
        (recur (inc i) st' (conj hist st'))))))

(deftest the-battle-starts-with-two-fleets
  (let [st (w/initial-state)]
    (testing "two armed hulls on opposing headings"
      (is (= :playing (:phase st)))
      (is (= #{:player :enemy} (set (keys (:ships st)))))
      (is (empty? (:shells st)))
      (is (zero? (:time st)))
      (is (true? (get-in st [:ships :enemy :ai])))
      (is (not (get-in st [:ships :player :ai])))
      (doseq [s (vals (:ships st))]
        (is (> (count (:cells s)) 100) "a substantial hull")
        (is (= (count (buoy/surface-faces (:cells s))) (count (:skin s))))
        (is (zero? (:cooldown s)))
        (is (not (:sunk s))))
      (let [pp (get-in st [:ships :player :pos])
            ep (get-in st [:ships :enemy :pos])]
        (is (> (Math/abs (- (pp 2) (ep 2))) 10.0) "fleets start apart")))
    (testing "over calm water"
      (let [ps (get-in st [:ocean :particles])]
        (is (>= (count ps) 90))
        (is (every? #(zero? (:omega %)) ps))))))

(deftest guns-solve-a-low-arc-onto-the-aim-point
  (let [target [0.0 0.5 0.0]
        st (w/fire (rigged) :player target)]
    (is (= 1 (count (:shells st))) "one shell away")
    (is (pos? (get-in st [:ships :player :cooldown])) "the gun cools down")
    (is (= st (w/fire st :player target)) "cooling guns hold fire")
    (let [hist (cruise st 300 0.02)
          flights (keep (fn [s] (first (:shells s))) hist)]
      (is (empty? (:shells (peek hist))) "the shell splashes and is spent")
      (is (< (apply min (map #(dist (:pos %) target) flights)) 1.0)
          "the arc passes within a metre of the aim point")))
  (testing "out of range there is no solution"
    (let [st (w/fire (rigged) :player [0.0 1.0 300.0])]
      (is (empty? (:shells st)))
      (is (zero? (get-in st [:ships :player :cooldown]))))))

(deftest shells-carve-the-hull-where-they-land
  (let [st (w/fire (rigged) :player [0.0 1.0 14.0])
        ;; 6 s window: the enemy's second salvo (3 s cooldown) must land
        hist (cruise st 60 0.1)
        final (peek hist)]
    (is (empty? (:shells final)) "the shell is spent on the enemy")
    (is (< (count (get-in final [:ships :enemy :cells]))
           (count (get-in st [:ships :enemy :cells]))))
    (is (some (fn [s] (some (fn [e] (and (= :blast (:type e))
                                         (= :enemy (:ship e))))
                            (:events s)))
              hist))))

(deftest hulls-carry-their-live-skin-for-rendering
  (let [st (rigged)
        fired (w/fire st :player [0.0 1.0 14.0])
        hist (cruise fired 40 0.1)
        final (peek hist)]
    (is (= (count (buoy/surface-faces (get-in st [:ships :enemy :cells])))
           (count (get-in st [:ships :enemy :faces])))
        "intact hulls cache their exposed faces")
    (is (= (count (buoy/surface-faces (get-in final [:ships :enemy :cells])))
           (count (get-in final [:ships :enemy :faces])))
        "a blast re-derives the live face cache from the carved cells")))

(deftest the-enemy-returns-fire
  (testing "in range it opens up on the player"
    (let [st0 (rigged)
          st (w/step-state st0 0.1 (facts-for st0))]
      (is (= 1 (count (:shells st))))
      (is (pos? (get-in st [:ships :enemy :cooldown])))))
  (testing "beyond ballistic range it holds fire"
    (let [far (assoc-in (rigged) [:ships :enemy :pos] [0.0 -1.0 200.0])]
      (is (empty? (:shells (w/step-state far 0.1 (facts-for far)))))))
  (testing "a sunk foe is not engaged"
    (let [st (assoc-in (rigged) [:ships :player :sunk] true)]
      (is (empty? (:shells (w/step-state st 0.1 (facts-for st))))))))

(deftest a-ship-that-goes-deep-is-sunk
  (let [st0 (rigged)
        facts {:bodies [{:body 101 :pos [0.0 -1.0 -14.0] :quat [0.0 0.0 0.0 1.0]
                         :vel [0.0 0.0 0.0] :speed 0.0 :asleep false}
                        {:body 102 :pos [0.0 -20.0 14.0] :quat [0.0 0.0 0.0 1.0]
                         :vel [0.0 0.0 0.0] :speed 0.0 :asleep false}]}
        st (w/step-state st0 0.1 facts)]
    (is (true? (get-in st [:ships :enemy :sunk])))
    (is (= :over (:phase st)))
    (is (= :player (:winner st)))
    (is (some #(and (= :sunk (:type %)) (= :enemy (:ship %))) (:events st)))))

(deftest the-sea-feels-the-battle
  (let [st (w/fire (rigged) :player [0.0 0.5 0.0])
        hist (cruise st 60 0.1)
        final (peek hist)]
    (is (some (fn [s] (some (fn [e] (= :splash (:type e))) (:events s))) hist)
        "the water impact is narrated")
    (is (some #(not (zero? (:omega %))) (get-in final [:ocean :particles]))
        "the splash leaves swirl in the water")))

(deftest physics-wiring
  (let [st (w/attach-bodies (w/initial-state) {:player 101 :enemy 102})]
    (is (= 101 (get-in st [:ships :player :body])))
    (is (= 102 (get-in st [:ships :enemy :body])))
    (is (= #{101 102} (w/live-body-ids st)))))

(deftest fleets-spawn-out-of-gun-range
  (let [p (get-in (w/initial-state) [:ships :player :pos])
        e (get-in (w/initial-state) [:ships :enemy :pos])
        d (Math/sqrt (reduce + (map #(* % %) (map - p e))))]
    (is (>= d 40.0)
        "fleets start beyond the 36-unit low-arc gun range - sail in to engage")))

(deftest the-sea-covers-the-whole-arena
  (let [st (w/initial-state)
        ps (get-in st [:ocean :particles])]
    (is (>= (count ps) 1500) "one continuous particle sheet, not a patch")
    (doseq [s (vals (:ships st))]
      (let [[px _ pz] (:pos s)]
        (is (some (fn [p] (and (< (Math/abs (- (:x p) px)) 1.5)
                               (< (Math/abs (- (:z p) pz)) 1.5))) ps)
            "water is under every hull from the first frame")))))
