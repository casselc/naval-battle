(ns voxel.ship-test
  "The warship layout as pure data: a contiguous hull, a tapered bow, guns
  mounted on deck, and a body that actually displaces water when posed."
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.ship :as ship]
            [voxel.buoyancy :as buoy]))

(defn- contiguous?
  "Every cell reachable from the first by face-adjacency."
  [cells]
  (let [cs (set (keys cells))]
    (loop [seen #{(first cs)} frontier [(first cs)]]
      (if (empty? frontier)
        (= (count seen) (count cs))
        (let [nbrs (for [d [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]
                     :let [n (mapv + (first frontier) d)]
                     :when (and (contains? cs n) (not (contains? seen n)))]
                   n)]
          (recur (into seen nbrs) (into (rest frontier) nbrs)))))))

(defn- beam-at
  "Hull beam (x extent) at column k."
  [cells k]
  (let [xs (for [[i _ kk] (keys cells) :when (= kk k)] i)]
    (if (empty? xs) 0 (- (apply max xs) (apply min xs) -1))))

(deftest dreadnought-is-one-contiguous-structure
  (let [{:keys [cells]} (ship/dreadnought)]
    (is (> (count cells) 100) "a substantial ship")
    (is (contiguous? cells) "no floating fragments - one welded hull")))

(deftest hull-is-longer-than-wide-and-tapers-at-the-bow
  (let [{:keys [cells]} (ship/dreadnought)
        ks (map #(nth % 2) (keys cells))
        length (- (apply max ks) (apply min ks) -1)
        midships (beam-at cells 13)
        bow (beam-at cells 25)]
    (is (> length (* 2.5 midships)) "a ship, not a barge")
    (is (< bow midships) "the prow narrows")
    (is (pos? bow) "but the bow still has substance")))

(deftest guns-are-mounted-on-deck-under-open-sky
  (let [{:keys [cells guns]} (ship/dreadnought)]
    (is (>= (count guns) 2) "at least a fore and an aft turret")
    (doseq [g guns]
      (testing (str "turret at " g)
        (is (contains? cells g) "the turret is part of the ship")
        (is (contains? cells (mapv - g [0 1 0])) "rests on structure below")
        (is (not (contains? cells (mapv + g [0 1 0]))) "nothing blocks it above")))))

(deftest the-hull-displaces-water-at-draft
  (let [ship (ship/dreadnought)
        posed {:cells (:cells ship)
               :anchor (:anchor ship)
               :pos [0.0 -1.0 0.0]
               :quat [0.0 0.0 0.0 1.0]}
        {:keys [volume centroid]} (buoy/submerged-metrics posed)
        {:keys [force]} (buoy/buoyancy-force posed)]
    (is (pos? volume) "keel a metre down means real displacement")
    (is (> (nth centroid 1) -2.0) "centre of buoyancy within the draft")
    (is (and (zero? (force 0)) (pos? (force 1)) (zero? (force 2)))
        "pure uplift")
    (is (< (force 1) (* buoy/WATER-DENSITY buoy/GRAVITY (count (:cells ship))))
        "displaces less than the whole ship's volume")))
