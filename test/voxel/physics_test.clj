(ns voxel.physics-test
  "The ocean world end to end: no ground (things sink out of sight), buoyancy
  floats voxel bodies at their draft, breaches flood and sink, and every
  floating body is driven independently (multi-shell crews of bodies)."
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.physics :as phys]
            [voxel.mesh :as mesh]
            [voxel.ship :as ship]
            [voxel.world :as w]
            [voxel.box3d :as b3]
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
                             (:anchor layout) (keys (:cells layout))
                             nil (:voxel layout))
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
                             (:anchor layout) (keys (:cells layout))
                             nil (:voxel layout))
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
                             (:anchor layout) (keys (:cells layout))
                             nil (:voxel layout))]
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


;; --- can she be missed? -----------------------------------------------------
;;
;; A shell is aimed where the target would be if she kept doing what she is
;; doing. Whether evasion means anything is therefore one number: how far off
;; that straight-line prediction can she get before the shell arrives, against
;; her own beam. These pin it, because it is easy to lose by accident - a
;; heavier damping constant or a slower shell and manoeuvring stops mattering.

(defn- warship []
  (let [l (ship/dreadnought)]
    (phys/spawn-body! [0.0 -3.0 0.0] (buoy/yaw-quat 0.0) 1
                      (:anchor l) (keys (:cells l)) nil (:voxel l))))

(defn- track-off
  "How far the hull ends up from where a gunner would have predicted, having
  steered `turn` for one shell's time of flight at `rng`."
  [rng turn]
  (phys/init!)
  (let [id (warship)
        _ (settle 3.0 0.02)
        _ (sail id 6.0 0.02 1.0 0.0)
        f0 (body-fact (phys/step! 0.02) id)
        [px _ pz] (:pos f0)
        [vx _ vz] (:vel f0)
        t (w/flight-time rng)
        [ax _ az] (:pos (body-fact (sail id t 0.02 1.0 turn) id))]
    [(Math/sqrt (+ (* (- ax (+ px (* vx t))) (- ax (+ px (* vx t))))
                   (* (- az (+ pz (* vz t))) (- az (+ pz (* vz t))))))
     (:speed f0)]))

(deftest a-ship-can-steer-out-of-a-firing-solution
  (testing "at her standoff range a hard turn takes her clear of her own beam"
    (let [[off spd] (track-off w/STANDOFF 1.0)]
      (is (> spd 6.0) (str "she has way on: " spd))
      (is (> off 3.5)
          (str "hard helm leaves the predicted point by " off
               " - her half-beam is 3.5, so the shell misses"))))
  (testing "holding course lands her where the gunner said she would be"
    (let [[off _] (track-off w/STANDOFF 0.0)]
      (is (< off 1.0)
          (str "steady course deviates only " off " - the lead is good"))))
  (testing "and closing the range takes that away, which is why the AI does
            not want a knife fight"
    (let [[far _] (track-off w/STANDOFF 1.0)
          [near _] (track-off w/KNIFE-RANGE 1.0)]
      (is (< near far)
          (str "at knife range the same helm only buys " near
               " against " far " at standoff")))))

(deftest engines-drive-a-hull-straight
  (testing "full ahead with the helm amidships holds her heading"
    ;; propulsion is applied through the centre of mass; through the grid
    ;; anchor instead - half a cell off the centreline and two below the
    ;; mass - it put a permanent couple on her and she steamed in a circle
    (phys/init!)
    (let [id (warship)]
      (settle 3.0 0.02)
      (let [y0 (yaw-of (body-fact (phys/step! 0.02) id))
            y1 (yaw-of (body-fact (sail id 6.0 0.02 1.0 0.0) id))]
        (is (< (Math/abs (- y1 y0)) 0.05)
            (str "yawed " (Math/toDegrees (- y1 y0)) " deg under power alone")))))
  (testing "and the helm still bites"
    (phys/init!)
    (let [id (warship)]
      (settle 3.0 0.02)
      (sail id 3.0 0.02 1.0 0.0)
      (let [y0 (yaw-of (body-fact (phys/step! 0.02) id))
            y1 (yaw-of (body-fact (sail id 3.0 0.02 1.0 1.0) id))
            rate (Math/toDegrees (/ (- y1 y0) 3.0))]
        (is (< 12.0 rate 40.0)
            (str "turn rate " rate " deg/s - fast enough to dodge, slow "
                 "enough to still be a warship"))))))

(deftest a-hull-resists-moving-sideways
  (testing "she slides far less across the beam than she runs along the keel"
    ;; without this the body slides sideways as freely as forward, so putting
    ;; the helm over swings the bow while momentum carries her along the old
    ;; track, and turning stops being evasion
    (phys/init!)
    (let [id (warship)
          _ (settle 3.0 0.02)
          _ (phys/step! 0.02)]
      ;; kick her squarely sideways and see how much of it survives
      (b3/set-velocity! id 6.0 0.0 0.0)
      (let [[sx _ _] (:vel (body-fact (settle 1.0 0.02) id))]
        (b3/set-velocity! id 0.0 0.0 6.0)
        (let [[_ _ sz] (:vel (body-fact (settle 1.0 0.02) id))]
          (is (pos? sz) "she keeps way along the keel")
          (is (< (Math/abs sx) (* 0.4 sz))
              (str "a second after a 6 u/s kick she has " sx " left across "
                   "the beam against " sz " along the keel")))))))

(deftest damage-refreshes-the-live-skin
  (testing "the exposed faces come back from the kernel after a hit, so the
            renderer's hull follows the damage"
    (phys/init!)
    (let [id (warship)
          before (:faces (body-fact (phys/step! 0.02) id))
          layout (ship/dreadnought)
          ;; scoop out a block amidships
          doomed (filter (fn [[i j k]]
                           (and (< (Math/abs (- k (quot ship/LENGTH 2))) 4)
                                (> j (- ship/DEPTH 4))))
                         (keys (:cells layout)))]
      (is (seq before))
      (phys/damage-cells! id (set doomed))
      (let [after (:faces (body-fact (phys/step! 0.02) id))]
        (is (seq after))
        (is (not= before after) "the skin changed where the hull did")))))

;; --- she rides, she does not bob --------------------------------------------

(deftest a-hull-settles-instead-of-bobbing
  (testing "pushed under and released, she comes back and stays there"
    ;; without drag against vertical motion a floating body is a cork: it
    ;; oscillates about its waterline for as long as you care to watch
    (phys/init!)
    (let [id (warship)
          _ (settle 12.0 0.02)
          y0 (get-in (body-fact (phys/step! 0.02) id) [:pos 1])
          _ (b3/set-velocity! id 0.0 -4.0 0.0)
          trace (mapv (fn [_]
                        (let [f (body-fact (settle 0.4 0.02) id)]
                          (- (get-in f [:pos 1]) y0)))
                      (range 12))
          late (drop 6 trace)]
      (is (< (apply min trace) -0.3) "she goes under from the push")
      (is (every? #(< (Math/abs %) 0.25) late)
          (str "and is back on her waterline within two seconds: " (vec late)))
      (is (< (apply max (map #(Math/abs %) late))
             (* 0.4 (apply max (map #(Math/abs %) (take 4 trace)))))
          "each swing is much smaller than the last, not a ringing cork"))))

(deftest a-warship-is-heavy-and-draws-deep
  (testing "she floats with most of her hull under and a little freeboard"
    (phys/init!)
    (let [id (warship)
          y (get-in (body-fact (settle 12.0 0.02) id) [:pos 1])
          draft (- y)]
      (is (< (* 0.55 ship/DEPTH-U) draft (* 0.9 ship/DEPTH-U))
          (str "draws " draft " of a " ship/DEPTH-U "-unit hull"))
      (is (> (+ ship/DEPTH-U y) 0.5) "and keeps a deck edge above water"))))

(deftest a-fine-hull-is-a-handful-of-collision-solids
  (testing "the physics body follows the hull's shape, not its cell count"
    ;; one solid per voxel is what a coarse ship could get away with; the
    ;; same hull at half-size voxels is eight times the cells and no solver
    ;; wants tens of thousands of shapes for two ships
    (let [cells (:cells (ship/dreadnought))
          boxes (mesh/solid-boxes cells)
          filled (reduce + (map (fn [[i0 j0 k0 i1 j1 k1]]
                                  (* (- i1 i0) (- j1 j0) (- k1 k0)))
                                boxes))]
      (is (= (count cells) filled)
          "the boxes tile the voxels exactly - same shape, same mass")
      (is (< (count boxes) (/ (count cells) 100))
          (str (count boxes) " solids for " (count cells) " voxels")))))
