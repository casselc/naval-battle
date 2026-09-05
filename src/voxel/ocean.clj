(ns voxel.ocean
  "The particle ocean, per the plan: water as individually interacting fluid
  particles, with the fast multipole method grouping distant particles so the
  sum over the whole sea costs O(N) instead of O(N^2).

  The physical model is a 2D vortex-particle method - the incompressible
  surface flow written on Lagrangian particles that carry vorticity omega.
  The velocity of every particle is the Biot-Savart sum over every vortex,
  which is where the FMM comes in:

  - w = u_x + i u_z of a CCW vortex G at the origin is (G/2pi) i / conj(z),
    so the whole field is the analytic function
      f(zeta) = SUM_j q_j / (zeta - zeta_j),   q_j = i G_j / 2pi,
    evaluated at zeta = conj(z) - the standard 2D Laplace FMM kernel.

  - Quadtree over the domain. Leaves hold particles; the upward pass builds
    complex multipole expansions a_k = SUM q_j (zeta_j - c)^k, M2M merges
    children into parents, the downward pass turns well-separated cells into
    local Taylor expansions (M2L) and shifts them down (L2L), and each
    particle reads the field from its leaf's local expansion plus direct
    summation against touching leaves only.

  - The field is the rotated gradient of a stream function, so it is
    divergence-free by construction: incompressible water, the way the plan
    describes. Blasts and ship wakes inject vorticity that stays coherent as
    visible swirls instead of diffusing into noise.

  Vertical dynamics is spray: particles thrown above the surface (blast
  impulses, hull pushes) fly ballistically and splash back to y=0.

  Everything is pure data ({:particles [...]}) and headless-testable.")

(def BOUNDS 44.0)
(def LEAF-MAX 12)
(def MAX-DEPTH 6)
(def EXPANSION-P 10)
(def VISCOSITY 0.04)
(def SPRAY-G 20.0)
(def IMPULSE-DRAG 0.90)

;; --- complex numbers as [re im] ---------------------------------------------------

(defn- c+ [[a b] [c d]] [(+ a c) (+ b d)])
(defn- c-neg [[a b]] [(- a) (- b)])
(defn- c* [[a b] [c d]] [(- (* a c) (* b d)) (+ (* a d) (* b c))])
(defn- c*real [s [a b]] [(* s a) (* s b)])
(defn- c-conj [[a b]] [a (- b)])
(defn- c-abs [[a b]] (Math/sqrt (+ (* a a) (* b b))))

(defn- c-inv
  "1/z."
  [[a b]]
  (let [d (+ (* a a) (* b b))]
    [(/ a d) (/ (- b) d)]))

(def ^:private binomial-cache (volatile! {}))

(defn- binomial
  [n k]
  (let [key [n k]]
    (or (get @binomial-cache key)
        (let [v (if (or (< k 0) (> k n))
                  0.0
                  (loop [i 1 acc 1.0]
                    (if (> i k)
                      acc
                      (recur (inc i) (* acc (/ (- n (- i 1)) i))))))]
          (vswap! binomial-cache assoc key v)
          v))))

;; --- ocean state -------------------------------------------------------------------

(defn make-ocean
  "Ocean from particle specs ({:x :z :omega ...} with defaults). :bounds is
  the reflecting domain half-width; positions clamp into it so every particle
  lives inside the FMM domain (the sea is a bounded arena)."
  [specs]
  {:particles (mapv (fn [s]
                      (let [p (merge {:x 0.0 :y 0.0 :z 0.0 :vx 0.0 :vy 0.0 :vz 0.0
                                      :omega 0.0}
                                     s)
                            cl (fn [v] (max (- BOUNDS) (min BOUNDS v)))]
                        (assoc p :x (cl (:x p)) :z (cl (:z p)))))
                    specs)
   :bounds BOUNDS
   :viscosity 0.0
   :p EXPANSION-P})

(defn total-circulation
  [oc]
  (reduce + (map :omega (:particles oc))))

;; --- the field: direct summation (oracle + FMM near-field) -------------------------

(defn- conj-zeta
  "Conjugated particle position as complex [re im] = [x, -z]."
  [p]
  [(:x p) (- (:z p))])

(defn- charges
  "Complex vortex charges: q_j = i omega_j / 2pi."
  [ps]
  (mapv (fn [p] (c*real (/ (:omega p) (* 2.0 Math/PI)) [0.0 1.0])) ps))

(defn direct-velocities
  "Every particle's velocity from the full O(N^2) Biot-Savart sum, as
  [[ux 0.0 uz] ...]. Self terms excluded.

  Written as a primitive component loop: with zeta = x - i.z and
  q_j = i.omega_j/2pi, q_j/(zeta_i - zeta_j) works out to
  [-omega_j (z_i - z_j), omega_j (x_i - x_j)] / (2pi r^2) - no complex
  helpers, no per-pair allocation."
  [oc]
  (let [ps (:particles oc)
        n (count ps)
        xs (mapv :x ps)
        zs (mapv :z ps)
        ks (mapv (fn [p] (/ (:omega p) (* 2.0 Math/PI))) ps)]
    (mapv (fn [i]
            (let [xi (xs i) zi (zs i)]
              (loop [j 0 fx 0.0 fz 0.0]
                (if (== j n)
                  [fx 0.0 fz]
                  (if (== j i)
                    (recur (inc j) fx fz)
                    (let [dx (- xi (xs j))
                          dz (- zi (zs j))
                          r2 (+ (* dx dx) (* dz dz))]
                      (if (< r2 1e-24)
                        (recur (inc j) fx fz)
                        (let [k (ks j)]
                          (recur (inc j)
                                 (- fx (/ (* k dz) r2))
                                 (+ fz (/ (* k dx) r2)))))))))))
          (range n))))

;; --- quadtree ------------------------------------------------------------------------

(defn- decode-cell
  "Center and half-width of the cell at quadrant path k (digits 0..3:
  0 (-x,-z) 1 (+x,-z) 2 (-x,+z) 3 (+x,+z); x-side = odd digit, z-side >= 2)
  in a bounds-half-width domain."
  [k bounds]
  (loop [cx 0.0 cz 0.0 h bounds k k]
    (if (empty? k)
      [cx cz h]
      (let [d (first k)
            h2 (* 0.5 h)]
        (recur (+ cx (* (if (odd? d) 1.0 -1.0) h2))
               (+ cz (* (if (>= d 2) 1.0 -1.0) h2))
               h2 (rest k))))))

(defn- quadrant-of
  "Which child quadrant of the cell at [cx cz] contains [x z]."
  [cx cz x z]
  (if (> z cz)
    (if (> x cx) 3 2)
    (if (> x cx) 1 0)))

(defn build-tree
  "Quadtree over the ocean's particles, as a flat map {path cell}; every cell
  {:path :cx :cz :h :leaf :idxs :kids}. Positions clamp into the domain so
  every particle lands in a leaf."
  [oc]
  (let [b (:bounds oc)
        ps (:particles oc)
        clamp (fn [v] (max (- b) (min b v)))
        pos (fn [i] [(clamp (:x (ps i))) (clamp (:z (ps i)))])
        build (fn build [idxs cx cz h depth path]
                (if (or (<= (count idxs) LEAF-MAX) (>= depth MAX-DEPTH))
                  {:path path :cx cx :cz cz :h h :leaf true :idxs idxs :kids []}
                  (let [buckets (reduce (fn [m i]
                                          (let [[x z] (pos i)
                                                q (quadrant-of cx cz x z)]
                                            (assoc m q (conj (get m q []) i))))
                                        {} idxs)
                        h2 (* 0.5 h)]
                    {:path path :cx cx :cz cz :h h :leaf false :idxs []
                     :kids (mapv (fn [q]
                                   (let [sx (if (odd? q) 1.0 -1.0)
                                         sz (if (>= q 2) 1.0 -1.0)]
                                     (build (get buckets q [])
                                            (+ cx (* sx h2)) (+ cz (* sz h2))
                                            h2 (inc depth) (conj path q))))
                                 [0 1 2 3])})))
        root (build (vec (range (count ps))) 0.0 0.0 b 0 [])]
    (into {} (map (fn [c] [(:path c) c])
                  (tree-seq (fn [c] (seq (:kids c))) :kids root)))))

(defn- touching?
  "Same-level cells whose closed squares share any boundary point."
  [c1 c2]
  (and (<= (Math/abs (- (:cx c1) (:cx c2))) (+ (:h c1) (:h c2) 1e-12))
       (<= (Math/abs (- (:cz c1) (:cz c2))) (+ (:h c1) (:h c2) 1e-12))))

(def ^:dynamic *sep-units* 2.0)

(defn- sep-units
  "Chebyshev centre separation of two same-level cells, in units of their
  half-width h. Touching cells read 2.0; one full cell of gap reads 4.0."
  [c1 c2]
  (/ (max (Math/abs (- (:cx c1) (:cx c2)))
          (Math/abs (- (:cz c1) (:cz c2))))
     (:h c1)))

(defn- adjacent-leaves?
  "Leaves are near-field neighbours when their cells, compared at the coarser
  of their two levels (adaptive trees put neighbours at different depths),
  lie within SEP-UNITS. Pairs not adjacent this way are separated by a real
  gap and are handled by the multipole passes instead."
  [bounds l1 l2]
  (let [lvl (min (count (:path l1)) (count (:path l2)))
        c1 (decode-cell (take lvl (:path l1)) bounds)
        c2 (decode-cell (take lvl (:path l2)) bounds)]
    (<= (max (Math/abs (- (first c1) (first c2)))
             (Math/abs (- (second c1) (second c2))))
        (* *sep-units* (nth c1 2)))))

;; --- FMM passes -----------------------------------------------------------------------

(defn- leaf-multipole
  "P2M: a_k = SUM_j q_j (zeta_j - c)^k for k in 0..p-1."
  [cell zetas qs p]
  (let [c [(:cx cell) (- (:cz cell))]
        acc (volatile! (vec (repeat p [0.0 0.0])))]
    (doseq [i (:idxs cell)]
      (let [d (c+ (zetas i) (c-neg c))
            q (qs i)]
        (vswap! acc update 0 c+ q)
        (loop [k 1 zk d]
          (when (< k p)
            (vswap! acc update k c+ (c* q zk))
            (recur (inc k) (c* zk d))))))
    @acc))

(defn- m2m
  "Merge child multipole into the parent accumulator.
  a'_k = SUM_{m+l=k} a_m (-1)^l C(k,l) delta^l, delta = center(parent-child).
  The l=0 term is the child's own a_k, added onto the running accumulator."
  [parent-acc child-a delta p]
  (mapv (fn [k a-k]
          (loop [l 1 dl delta acc (c+ a-k (child-a k))]
            (if (> l k)
              acc
              (recur (inc l)
                     (c* dl delta)
                     (c+ acc (c*real (* (Math/pow -1 l) (binomial k l))
                                    (c* dl (child-a (- k l)))))))))
        (range p) parent-acc))

(defn- m2l
  "Add source multipole (center c) into target local expansion (center C):
  b_k += SUM_m (-1)^(m+1) C(m+k,k) a_m (c-C)^(-m-1-k)."
  [b a-src delta p]
  (let [d-inv (c-inv delta)
        r (loop [m 0 d d-inv out []]
            (if (>= m p)
              out
              (recur (inc m) (c* d d-inv) (conj out d))))
        dk (loop [k 0 d [1.0 0.0] out []]
            (if (>= k p)
              out
              (recur (inc k) (c* d d-inv) (conj out d))))]
    (loop [k 0 acc b]
      (if (>= k p)
        acc
        (recur (inc k)
               (assoc acc k
                      (c+ (acc k)
                          (c* (nth dk k)
                              (loop [m 0 sum [0.0 0.0]]
                                (if (>= m p)
                                  sum
                                  (recur (inc m)
                                         (c+ sum
                                             (c*real (* (if (odd? m) 1.0 -1.0)
                                                        (binomial (+ m k) k))
                                                   (c* (nth r m) (a-src m)))))))))))))))

(defn- l2l
  "Shift local expansion by mu = new-center - old-center:
  b'_k = SUM_{m>=k} C(m,k) mu^(m-k) b_m."
  [b mu p]
  (mapv (fn [k]
          (loop [m k acc [0.0 0.0] dm [1.0 0.0]]
            (if (>= m p)
              acc
              (recur (inc m)
                     (c+ acc (c*real (binomial m k) (c* (b m) dm)))
                     (c* dm mu)))))
        (range p)))

(defn- eval-local
  "f(zeta) = SUM_k b_k (zeta - C)^k."
  [b c zeta p]
  (loop [k 0 acc [0.0 0.0] term [1.0 0.0]]
    (if (>= k p)
      acc
      (recur (inc k)
             (c+ acc (c* (b k) term))
             (c* term (c+ zeta (c-neg c)))))))

(defn- path-sep-units
  "Chebyshev centre separation of the cells at two same-level paths, in
  units of their half-width."
  [bounds p1 p2]
  (let [[x1 z1 h1] (decode-cell p1 bounds)
        [x2 z2 _] (decode-cell p2 bounds)]
    (/ (max (Math/abs (- x1 x2)) (Math/abs (- z1 z2))) h1)))

(defn- ccenter [bounds path]
  (let [[x z] (decode-cell path bounds)]
    [x (- z)]))

(defn fmm-velocities
  "Every particle's velocity via the FMM: its leaf's local expansion plus
  direct sums against touching leaves only. Agrees with direct-velocities
  to ~1e-4 on random clouds (see tests)."
  [oc]
  (let [ps (:particles oc)
        p (:p oc)
        bounds (:bounds oc)
        zetas (mapv conj-zeta ps)
        qs (charges ps)
        tree (build-tree oc)
        cells (vec (vals tree))
        by-level (group-by (comp count :path) cells)
        levels (sort (keys by-level))
        ;; --- upward pass
        mult (reduce (fn [m cell]
                       (if (:leaf cell)
                         (assoc m (:path cell) (leaf-multipole cell zetas qs p))
                         m))
                     {} cells)
        mult (reduce (fn [m depth]
                       (reduce (fn [m cell]
                                  (if (:leaf cell)
                                    m
                                    (assoc m (:path cell)
                                           (reduce (fn [acc kid]
                                                     (let [ka (get m (:path kid))]
                                                       (if ka
                                                         (m2m acc ka
                                                              (c+ (ccenter bounds (:path cell))
                                                                  (c-neg (ccenter bounds (:path kid))))
                                                              p)
                                                         acc)))
                                                   (vec (repeat p [0.0 0.0]))
                                                   (:kids cell)))))
                               m (get by-level depth)))
                     mult (rest (reverse levels)))
        ;; --- downward pass (interaction lists per level)
        locals (reduce (fn [loc depth]
                         (reduce (fn [loc cell]
                                   (let [cc (ccenter bounds (:path cell))
                                         inherited (if (empty? (:path cell))
                                                     (vec (repeat p [0.0 0.0]))
                                                     (l2l (get loc (pop (:path cell)))
                                                          (c+ cc (c-neg (ccenter bounds (pop (:path cell)))))
                                                          p))
                                          others (remove (fn [x] (= (:path x) (:path cell)))
                                                         (get by-level depth))
                                          ilist (filter (fn [src]
                                                          (and (> (sep-units src cell) *sep-units*)
                                                               (or (empty? (pop (:path cell)))
                                                                   (<= (path-sep-units bounds
                                                                                       (pop (:path cell))
                                                                                       (pop (:path src)))
                                                                       *sep-units*))))
                                                        others)
                                         b (reduce (fn [acc src]
                                                     (m2l acc (get mult (:path src))
                                                          (c+ (ccenter bounds (:path src))
                                                              (c-neg cc))
                                                          p))
                                                   inherited ilist)]
                                     (assoc loc (:path cell) b)))
                                 loc (get by-level depth)))
                       {} levels)
        ;; --- evaluation: local expansion + near-field direct
        leaves (filterv :leaf cells)
        ;; neighbour index lists are a property of the LEAF, not the
        ;; particle: compute them once per leaf or the adjacency scan runs
        ;; leaves x leaves per particle and swamps everything
        leaf-nbrs (into {} (map (fn [leaf]
                                  [(:path leaf)
                                   (vec (mapcat :idxs
                                                (filter (fn [o] (adjacent-leaves? bounds leaf o))
                                                        leaves)))])
                                leaves))
        out (object-array (count ps))
        near-sum (fn [leaf i zi]
                   (let [b (get locals (:path leaf))
                         c (ccenter bounds (:path leaf))
                         far (eval-local b c zi p)
                         js (get leaf-nbrs (:path leaf))]
                     (reduce (fn [f j]
                               (let [dz (c+ zi (c-neg (zetas j)))]
                                 (if (or (== j i) (< (c-abs dz) 1e-12))
                                   f
                                   (c+ f (c* (qs j) (c-inv dz))))))
                             far js)))]
    (doseq [leaf leaves]
      (doseq [i (:idxs leaf)]
        (let [w (near-sum leaf i (zetas i))]
          (aset out i [(w 0) 0.0 (w 1)]))))
    (vec out)))

;; --- stepping -------------------------------------------------------------------------

(defn- apply-blasts
  "Blasts [{:x :z :r :power}]: vertical kick + swirl injection falling off
  linearly to the blast radius."
  [ps blasts]
  (if (empty? blasts)
    ps
    (mapv (fn [p]
            (reduce (fn [p {:keys [x z r power]}]
                      (let [dx (- (:x p) x)
                            dz (- (:z p) z)
                            d (Math/sqrt (+ (* dx dx) (* dz dz)))]
                        (if (>= d r)
                          p
                          (let [w (- 1.0 (/ d r))]
                            (-> p
                                (update :vy + (* power w))
                                (update :omega + (* power 0.8 w)))))))
                    p blasts))
          ps)))

(defn- apply-hulls
  "Moving hulls [{:x :z :r :push :swirl}]: displace nearby water outward and
  shed vorticity at the hull sides (the wake)."
  [ps hulls]
  (if (empty? hulls)
    ps
    (mapv (fn [p]
            (reduce (fn [p {:keys [x z r push swirl]}]
                      (let [dx (- (:x p) x)
                            dz (- (:z p) z)
                            d (Math/sqrt (+ (* dx dx) (* dz dz)))]
                        (if (>= d r)
                          p
                          (let [w (- 1.0 (/ d r))
                                d (max d 1e-6)
                                nx (/ dx d)
                                nz (/ dz d)]
                            (-> p
                                (update :vx + (* push w nx))
                                (update :vz + (* push w nz))
                                (update :vy + (* 0.15 push w))
                                (update :omega + (* swirl w (- nz nx))))))))
                    p hulls))
          ps)))

(defn- step-vertical
  "Spray ballistic above the surface, splash-damped back onto it."
  [p dt]
  (let [y (+ (:y p) (* (:vy p) dt))
        vy (if (> (:y p) 0.0)
             (- (:vy p) (* SPRAY-G dt))
             (* 0.5 (:vy p)))]
    (if (< y 0.0)
      (assoc p :y 0.0 :vy 0.0)
      (assoc p :y y :vy vy))))

(def DIRECT-MAX 220)
(def ACTIVE-EPS 1e-9)   ; vorticity at or below this is still water
(def FIELD-RADIUS 14.0) ; the exact field reaches this far from any vortex
(def SPARSE-MAX 160)    ; more active vortices than this: dense churn, run the FMM
(def ^:private BUCKET 4.0) ; sparse-field target bucket width, world units

(defn- sparse-velocities
  "Exact Biot-Savart from the active vortices only, evaluated on the water
  within FIELD-RADIUS of one; beyond that the 1/r tail is under the noise
  and the sea rests. A battle agitates a few dozen particles at a time, so
  the whole-ocean cost is pairs-that-matter, not N^2. This pure loop is the
  REFERENCE - at runtime physics/init! swaps in the C kernel (voxel.seac)
  for the same sum at native speed."
  [ps]
  (let [act (vec (filter #(> (Math/abs (:omega %)) ACTIVE-EPS) ps))
        ax (mapv :x act)
        az (mapv :z act)
        ak (mapv #(/ (:omega %) (* 2.0 Math/PI)) act)
        m (count act)
        r2 (* FIELD-RADIUS FIELD-RADIUS)]
    (mapv (fn [p]
            (let [px (:x p) pz (:z p)]
              (loop [j 0 fx 0.0 fz 0.0]
                (if (== j m)
                  [fx 0.0 fz]
                  (let [dx (- px (ax j))
                        dz (- pz (az j))
                        d2 (+ (* dx dx) (* dz dz))]
                    (if (or (> d2 r2) (< d2 1e-24))
                      (recur (inc j) fx fz)
                      (let [k (ak j)]
                        (recur (inc j)
                               (- fx (/ (* k dz) d2))
                               (+ fz (/ (* k dx) d2))))))))))
          ps)))

(def field-kernel
  "Optional accelerator for the sparse field: physics/init! resets this to
  the C kernel (voxel.seac/field) at startup when the native library loads.
  Tests always run the pure reference above."

  (atom nil))

(defn velocities
  "The velocity field however it is cheapest at this sea state:
  - a still sea: no vortices, no field, no pair sums at all;
  - sparse vorticity (the usual battle): exact sums from the actives out to
    FIELD-RADIUS, still water beyond;
  - a fully agitated small sea: the direct O(N^2) sum, exact everywhere;
  - dense churn on a big sea: the FMM - the quadtree groups the whole sea so
    the cost stays O(N), which is the point of the fast algorithm."
  [oc]
  (let [ps (:particles oc)
        n (count ps)
        na (count (filter #(> (Math/abs (:omega %)) ACTIVE-EPS) ps))]
    (cond
      (zero? na) (vec (repeat n [0.0 0.0 0.0]))
      (<= na SPARSE-MAX) (if-let [k @field-kernel]
                           (k oc)
                           (sparse-velocities ps))
      (<= n DIRECT-MAX) (direct-velocities oc)
      :else (fmm-velocities oc))))

(defn step-ocean
  "Advance the ocean dt seconds. blasts and hulls are this frame's couplings
  from the world (see apply-blasts / apply-hulls)."
  ([oc dt] (step-ocean oc dt nil nil))
  ([oc dt blasts] (step-ocean oc dt blasts nil))
  ([oc dt blasts hulls]
   (let [ps (-> (:particles oc)
                (apply-blasts blasts)
                (apply-hulls hulls))
         field (velocities (assoc oc :particles ps))
         vis (or (:viscosity oc) VISCOSITY)
         b (:bounds oc)
         drag (Math/pow IMPULSE-DRAG dt)
         ps' (mapv (fn [p [fx _ fz]]
                     (if (and (zero? fx) (zero? fz)
                              (zero? (:vx p)) (zero? (:vz p))
                              (zero? (:vy p)) (zero? (:y p))
                              (<= (Math/abs (:omega p)) ACTIVE-EPS))
                       p
                       (let [vx (+ (:vx p) fx)
                           vz (+ (:vz p) fz)
                           x (+ (:x p) (* vx dt))
                           z (+ (:z p) (* vz dt))
                           reflect (fn [v s]
                                     (cond (> s b) [b (- v)]
                                           (< s (- b)) [(- b) (- v)]
                                           :else [s v]))
                           [x' vx'] (reflect vx x)
                           [z' vz'] (reflect vz z)
                           p' (assoc p :x x' :z z'
                                     :vx (* (- vx' fx) drag)
                                     :vz (* (- vz' fz) drag)
                                     :omega (* (- 1.0 (* vis dt)) (:omega p)))]
                       (step-vertical p' dt))))
                   ps field)]
     (assoc oc :particles ps' :time (+ (or (:time oc) 0.0) dt)))))
