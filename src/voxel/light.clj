(ns voxel.light
  "Pure shading and height-field mesh math for the renderer: the sea surface
  as one continuous sheet of shared-corner tiles with per-corner normals, sun
  shading with an ambient floor, and casting world points onto the field
  along the sun rays (hull shadows). Plain data in, plain data out - the
  renderer only turns the results into vertex calls.")

(def AMBIENT 0.35)

;; --- the shared-corner height field ----------------------------------------------

(defn corner-grid
  "Sample grid {[i j] {:h :foam ...}} -> shared corners {[ci cj] {:h :foam}},
  each corner the mean of the up-to-4 adjacent tiles. Adjacent tiles read the
  SAME corner, so the sheet is continuous with no gaps or steps. One
  accumulation pass - this runs every frame over the whole sea."
  [samples]
  (let [sums (reduce (fn [m [[i j] {:keys [h foam]}]]
                       (let [h (or h 0.0)
                             f (or foam 0.0)
                             add (fn [m c]
                                   (if-let [[hs fs n] (get m c)]
                                     (assoc m c [(+ hs h) (+ fs f) (inc n)])
                                     (assoc m c [h f 1])))]
                         (-> m
                             (add [i j])
                             (add [(inc i) j])
                             (add [i (inc j)])
                             (add [(inc i) (inc j)]))))
                     {} (map identity samples))]
    (into {} (map (fn [[c [hs fs n]]]
                    [c {:h (double (/ hs n)) :foam (double (/ fs n))}]))
          sums)))

(defn corner-normal
  "Up normal at corner [ci cj] from central differences over the corner height
  grid at lattice spacing s. Missing neighbours extrapolate flat (the corner's
  own height), so the sheet edge stays calm instead of creasing."
  [grid ci cj s]
  (let [own (fn [a b] (or (:h (get grid [a b])) (:h (get grid [ci cj])) 0.0))
        gx (/ (- (own (inc ci) cj) (own (dec ci) cj)) (* 2.0 s))
        gz (/ (- (own ci (inc cj)) (own ci (dec cj))) (* 2.0 s))
        l (Math/sqrt (+ 1.0 (* gx gx) (* gz gz)))]
    [(/ (- gx) l) (/ 1.0 l) (/ (- gz) l)]))

(defn corner-normals
  "corner-normal for every corner in one pass - the per-corner fn pays a
  closure and fallback lookups each call, which adds up over a whole sea."
  [grid s]
  (let [two-s (* 2.0 s)]
    (into {} (for [[c {:keys [h]}] grid
                   :let [[ci cj] c
                         hm (fn [a b] (or (:h (get grid [a b])) h))
                         gx (/ (- (hm (inc ci) cj) (hm (dec ci) cj)) two-s)
                         gz (/ (- (hm ci (inc cj)) (hm ci (dec cj))) two-s)
                         l (Math/sqrt (+ 1.0 (* gx gx) (* gz gz)))]]
               [c [(/ (- gx) l) (/ 1.0 l) (/ (- gz) l)]]))))

;; --- sun shading ----------------------------------------------------------------------

(defn sun-shade
  "Brightness in [AMBIENT, 1] for a surface normal against the unit vector
  toward the sun: faces aimed at the sun are full bright, everything else
  falls to the ambient floor."
  [n sun]
  (min 1.0 (+ AMBIENT (* (- 1.0 AMBIENT)
                         (max 0.0 (+ (* (n 0) (sun 0))
                                     (* (n 1) (sun 1))
                                     (* (n 2) (sun 2))))))))

;; --- shadow projection ------------------------------------------------------------------

(defn project-to-field
  "World point p cast along the sun rays (sun = unit vector TOWARD the sun)
  onto the height field h, a function of [x z]. One fixed-point step: solve
  against the flat sea, then sample the field where it lands. The result sits
  0.03 above the water so it never z-fights the surface."
  [p h sun]
  (let [[px py pz] p
        [sx sy sz] sun
        t (/ py sy)
        x' (- px (* t sx))
        z' (- pz (* t sz))]
    [x' (+ (h x' z') 0.03) z']))
