(ns voxel.world-test
  "The naval battle as pure state: two fleets, ballistic shells that carve
  hulls, an enemy that returns fire, a sea that feels every blast, and a
  winner when one ship goes under."
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.world :as w]
            [voxel.ocean :as sea]
            [voxel.buoyancy :as buoy]
            [voxel.ship :as ship]))

(defn- dist
  [a b]
  (Math/sqrt (reduce + (map #(* % %) (map - a b)))))

(defn- rigged
  "Initial state with both fleets attached to fabricated physics bodies, so
  physics facts fold by body id, posed at gun range so the duelling tests
  can actually hit each other."
  []
  (-> (w/initial-state)
      (assoc-in [:ships :player :pos] [0.0 -3.0 -14.0])
      (assoc-in [:ships :enemy :pos] [0.0 -3.0 14.0])
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
  intermediate state. The ocean is becalmed (:ambient false) so these
  gunnery tests pay the cheap still-sea path, not full ambient churn."
  [st n dt]
  (let [st (assoc-in st [:ocean :ambient] false)]
    (loop [i 0 st st hist []]
      (if (= i n)
        hist
        (let [st' (w/step-state st dt (facts-for st))]
          (recur (inc i) st' (conj hist st')))))))

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

(deftest fleets-spawn-clear-beyond-gun-range
  (let [p (get-in (w/initial-state) [:ships :player :pos])
        e (get-in (w/initial-state) [:ships :enemy :pos])
        d (Math/sqrt (reduce + (map #(* % %) (map - p e))))]
    (is (>= d (* 2.5 w/GUN-RANGE))
        (str "fleets start " d " apart against a " w/GUN-RANGE
             "-unit gun range - a proper approach, not a knife fight"))
    (is (< d (* 2.0 w/SEA-EXTENT))
        "and both start on the simulated sea")))

(deftest the-ai-keeps-clear-of-the-other-ship
  (testing "she breaks off before the hulls can touch - these are 26-unit
            ships, so a centre-to-centre range near that is already a
            collision, and ramming puts a hull under undamaged"
    (is (> w/KNIFE-RANGE ship/LENGTH)
        (str "break-off range " w/KNIFE-RANGE " against a "
             ship/LENGTH "-unit hull"))
    (is (> w/STANDOFF w/KNIFE-RANGE)
        "and she settles further out than that, not on the edge of it")))

(deftest the-fighting-band-is-inside-gun-range-but-not-a-brawl
  (is (< w/KNIFE-RANGE w/STANDOFF w/GUN-RANGE)
      "she wants to fight where she can shoot and still be missed")
  (is (> (w/flight-time w/STANDOFF) 2.0)
      (str "a shell takes " (w/flight-time w/STANDOFF)
           "s to reach her standoff - long enough to be somewhere else"))
  (testing "time of flight is what buys the dodge, and it grows with range -
            that is the whole reason she fights out here rather than closing"
    (is (> (w/flight-time w/STANDOFF) (w/flight-time (* 0.5 w/STANDOFF))))
    (is (< (w/flight-time (* 0.4 w/GUN-RANGE)) 1.6)
        "well inside, a shell arrives before a hull can move its own beam")))

(deftest the-sea-runs-past-the-frame
  (let [oc (:ocean (w/initial-state))
        ps (sea/particles oc)
        xs (map #(Math/abs (:x %)) ps)
        zs (map #(Math/abs (:z %)) ps)]
    (is (= (* w/SEA-COLS w/SEA-COLS) (count ps))
        "one particle per tile across the whole sheet")
    (is (>= (apply max xs) (- w/SEA-EXTENT 0.1))
        "the sheet reaches the full declared extent")
    (is (every? #(<= % w/SEA-BOUNDS) (concat xs zs))
        "no particle starts outside the ocean domain")
    (is (< (Math/abs (- w/SEA-SPACING w/SEA-TARGET-SPACING)) 0.2)
        "tiles come out near the target spacing")))

(defn- duel
  "A two-ship state with the AI enemy at the origin and the player placed at
  a given bearing and range, both making way."
  [range bearing & {:keys [foe-cooldown enemy-yaw player-vel]
                    :or {foe-cooldown 0.0 enemy-yaw 0.0}}]
  (-> (w/initial-state)
      (assoc-in [:ships :enemy :pos] [0.0 -3.0 0.0])
      (assoc-in [:ships :enemy :quat] (buoy/yaw-quat enemy-yaw))
      (assoc-in [:ships :player :pos] [(* range (Math/sin bearing)) -3.0
                                       (* range (Math/cos bearing))])
      (assoc-in [:ships :player :cooldown] foe-cooldown)
      (assoc-in [:ships :player :vel] (or player-vel [0.0 0.0 0.0]))))

(defn- helm-range-trend
  "Does this helm order open or close the range? Positive means opening."
  [st id]
  (let [[thrust _] (w/ai-helm st id)
        s (get-in st [:ships id])
        foe (first (remove #(= id (:id %)) (vals (:ships st))))
        want (w/ai-course st id)
        [dx _ dz] (mapv - (:pos foe) (:pos s))
        d (Math/sqrt (+ (* dx dx) (* dz dz)))]
    ;; component of the wanted course along the bearing to the foe
    (- (* thrust (+ (* (want 0) (/ dx d)) (* (want 1) (/ dz d)))))))

(deftest the-ai-closes-when-it-cannot-reach
  (testing "well outside gun range she comes on, under full power"
    (let [st (duel 90.0 0.0)]
      (is (= 1.0 (first (w/ai-helm st :enemy))) "full ahead")
      (is (neg? (helm-range-trend st :enemy)) "closing the range")))
  (testing "but not straight down the throat - she comes on at a slant, so
            she is already turning when she arrives"
    (let [st (duel 90.0 0.0)
          want (w/ai-course st :enemy)
          ;; bearing to the foe is +z; a pure charge would be [0 1]
          off (Math/abs (Math/atan2 (want 0) (want 1)))]
      (is (> off 0.15) (str "approach slant " off " rad")))))

(deftest the-ai-opens-the-range-when-it-is-too-close-to-dodge
  (testing "inside the range where a shell arrives before she can be
            elsewhere, she gets out rather than trading point blank"
    (let [st (duel (* 0.5 w/GUN-RANGE) 0.0)]
      (is (pos? (helm-range-trend st :enemy)) "opening the range")))
  (testing "and holds station once she is back in her fighting band"
    ;; at any instant she is 40 degrees off the tangent, because she is
    ;; weaving; it is the mean across the weave that keeps the range
    (let [mean (/ (+ (helm-range-trend (duel w/STANDOFF 0.0 :foe-cooldown 0.0)
                                       :enemy)
                     (helm-range-trend (duel w/STANDOFF 0.0
                                             :foe-cooldown (* 0.9 w/FIRE-COOLDOWN))
                                       :enemy))
                  2.0)]
      (is (< (Math/abs mean) 0.2)
          (str "neither closing nor running on average, got " mean))))
  (testing "the weave is a snake either side of her course, not a reversal -
            she cannot turn 180 degrees inside his reload, and trying leaves
            the helm saturated and the range running away"
    (let [a (w/ai-course (duel w/STANDOFF 0.0 :foe-cooldown 0.0) :enemy)
          b (w/ai-course (duel w/STANDOFF 0.0
                               :foe-cooldown (* 0.9 w/FIRE-COOLDOWN)) :enemy)
          dot (+ (* (a 0) (b 0)) (* (a 1) (b 1)))]
      (is (pos? dot)
          (str "the two weave legs still share a general direction, dot " dot)))))

(deftest the-ai-watches-the-other-ship-s-guns
  (testing "she reverses her helm across the foe's reload, so the cross-range
            speed he led on is not the one she is carrying when it lands"
    (let [loaded (duel w/STANDOFF 0.0 :foe-cooldown 0.0)
          fresh (duel w/STANDOFF 0.0 :foe-cooldown (* 0.9 w/FIRE-COOLDOWN))]
      (is (not= (w/ai-course loaded :enemy) (w/ai-course fresh :enemy))
          "a loaded enemy gun and a just-fired one are different problems")))
  (testing "a foe who cannot reach her is no threat at all"
    (is (nil? (w/gun-threat (get-in (duel 90.0 0.0) [:ships :enemy])
                            (get-in (duel 90.0 0.0) [:ships :player]))))
    (is (some? (w/gun-threat (get-in (duel 30.0 0.0) [:ships :enemy])
                             (get-in (duel 30.0 0.0) [:ships :player])))))
  (testing "a sunk foe has no guns"
    (let [st (assoc-in (duel 30.0 0.0) [:ships :player :sunk] true)]
      (is (nil? (w/gun-threat (get-in st [:ships :enemy])
                              (get-in st [:ships :player])))))))

(deftest the-ai-keeps-the-helm-working-under-threat
  (testing "with the foe's gun up she is always turning - a ship on a steady
            course is a ship the lead is already correct for"
    (doseq [bearing [0.0 1.1 2.2 3.3 4.4 5.5]]
      (let [st (duel w/STANDOFF bearing :foe-cooldown 0.0)
            [_ turn] (w/ai-helm st :enemy)]
        (is (not (zero? turn)) (str "bearing " bearing " helm amidships"))))))

(deftest a-dead-or-missing-foe-stops-the-ai
  (is (= [0.0 0.0] (w/ai-helm (assoc-in (duel 30.0 0.0) [:ships :player :sunk] true)
                              :enemy)))
  (is (= [0.0 0.0] (w/ai-helm (duel 30.0 0.0) :player)) "the player is not driven"))

;; --- gunnery: leading a moving target ---------------------------------------

(deftest the-gunner-leads-by-the-real-time-of-flight
  (testing "flight time is d/(v cos theta), not d/v"
    (doseq [d [12.0 24.0 33.0]]
      (is (> (w/flight-time d) (/ d w/SHELL-SPEED))
          (str "range " d ": a lofted shell is in the air longer than a flat one"))))
  (testing "the lead actually intercepts a target holding course"
    ;; fire at the aim point, fly the shell analytically, and see whether the
    ;; target has arrived at the same place
    (let [shooter {:pos [0.0 0.0 0.0]}
          target {:pos [0.0 0.0 30.0] :vel [7.0 0.0 0.0]}
          [ax _ az] (w/aim-point shooter target)
          d (Math/sqrt (+ (* ax ax) (* az az)))
          t (w/flight-time d)
          [px pz] [(+ (* 7.0 t)) 30.0]]
      (is (< (Math/abs (- ax px)) 0.6)
          (str "aim " ax " vs where she will be " px))
      (is (< (Math/abs (- az pz)) 0.6))))
  (testing "a stationary target needs no lead"
    (let [[ax _ az] (w/aim-point {:pos [0.0 0.0 0.0]}
                                 {:pos [5.0 0.0 20.0] :vel [0.0 0.0 0.0]})]
      (is (< (Math/abs (- ax 5.0)) 1e-9))
      (is (< (Math/abs (- az 20.0)) 1e-9)))))
