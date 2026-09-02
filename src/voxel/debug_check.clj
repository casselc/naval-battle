(ns voxel.debug-check
  (:require [voxel.ocean :as sea]))

(defn cloud [n seed]
  (let [s (atom seed)
        r (fn []
            (let [x (rem (+ (* 1103515245 @s) 12345) 2147483648)]
              (reset! s x)
              x))
        u (fn []
            (- (* 2.0 (/ (rem (r) 1000000) 1000000.0)) 1.0))]
    (sea/make-ocean (mapv (fn [_] {:x (u) :z (u) :omega (* 2.0 (u))}) (range n)))))

(defn contrib-of [ps i j]
  (let [pi (ps i)
        pj (ps j)
        dx (- (:x pi) (:x pj))
        dz (- (:z pi) (:z pj))
        r2 (+ (* dx dx) (* dz dz))
        k (/ (:omega pj) (* 2.0 Math/PI r2))]
    [(* k (- dz)) (* k dx)]))

(defn -main []
  (let [oc (cloud 500 99)
        f (sea/fmm-velocities oc)
        d (sea/direct-velocities oc)
        ps (:particles oc)
        diffs (map-indexed
                (fn [i dv]
                  [i (Math/hypot (- (first dv) (first (f i)))
                                 (- (nth dv 2) (nth (f i) 2)))])
                d)
        [wi we] (apply max-key second diffs)
        adj (ns-resolve 'voxel.ocean 'adjacent-leaves?)
        treef (ns-resolve 'voxel.ocean 'build-tree)
        tree (treef oc)
        leaves (filterv :leaf (vals tree))
        myleaf (first (filter (fn [l] (some #(= % wi) (:idxs l))) leaves))
        bounds (:bounds oc)
        near-js (set (mapcat :idxs (filter (fn [o] (adj bounds myleaf o)) leaves)))
        far-js (remove (fn [j] (or (contains? near-js j) (= j wi))) (range 500))
        far-sum (reduce (fn [acc j]
                          (let [c (contrib-of ps wi j)]
                            [(+ (first acc) (first c)) (+ (second acc) (second c))]))
                        [0.0 0.0]
                        far-js)
        rx (- (first (f wi)) (first (d wi)))
        rz (- (nth (f wi) 2) (nth (d wi)))]
    (println "worst" wi "err" we)
    (println "resid  " rx rz)
    (println "far-sum" far-sum)
    (println "far count" (count far-js) "near count" (count near-js))))
