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

(def BOUNDS 58.0)
(def LEAF-MAX 12)
(def MAX-DEPTH 6)
(def EXPANSION-P 10)
(def VISCOSITY 0.04)
(def SPRAY-G 20.0)

;; how the velocity field is evaluated at a given sea state
(def ACTIVE-EPS 1e-9)   ; vorticity at or below this is still water
(def FIELD-RADIUS 14.0) ; the exact field reaches this far from any vortex
(def SPARSE-MAX 2048)   ; actives up to this get exact sums; past it the FMM.
                        ; The ambient sea is fully active (every particle
                        ; carries gentle chop), so the live ocean rides the
                        ; FMM, which is the point of the fast algorithm.
(def DIRECT-MAX 220)    ; oracle only: direct-velocities is the test reference

;; ambient sea state: a travelling swell and slow turbulence eddies keep
;; the open water alive between battles. Both are deterministic functions
;; of (x, z, t), so every run - live or headless - shows the same sea.
(def SWELL-TRAINS
  "The ambient sea state, as [amplitude wavenumber frequency dir-x dir-z]
  per wave train. A single train forces every particle in step along one
  heading, which reads as a corrugated roof rather than open water; three at
  different wavelengths, headings and speeds interfere into something that
  looks like a sea. All of it forces the PARTICLES - the surface a player
  sees is still just where the particles are."
  [[0.45 0.55 1.30  0.86  0.51]    ; the main swell, ~11-unit wavelength
   [0.22 0.31 0.83 -0.42  0.91]    ; a long cross swell from the beam
   [0.11 1.15 2.10  0.62 -0.78]])  ; short wind chop across both
(def CHOP-RATE 0.35)        ; seconds between eddy reshuffles
(def CHOP-AMP 0.0009)       ; vorticity injected per second: visible swirl

(defn- chop-rand
  "Deterministic pseudo-random in [-1,1) from grid position and time
  bucket: the ambient turbulence field, reproducible everywhere."
  [x z k]
  (let [h (Math/abs (bit-xor (* (long (* x 8.0)) 374761393)
                             (* (long (* z 8.0)) 668265263)
                             (* (long k) 1274126177)))]
    (- (* 2.0 (/ (double (mod h 1000003)) 1000003.0)) 1.0)))

(defn- apply-ambient
  "The open sea is never glass: a travelling swell forces the surface and
  slow deterministic eddies stir gentle vorticity everywhere, so the whole
  ocean reads as an active particle system even before the guns speak.
  Becalmed oceans (:ambient false) skip this."
  [ps t dt]
  (let [kb (long (quot t CHOP-RATE))]
    (mapv (fn [p]
            (let [x (:x p) z (:z p)
                  lift (reduce (fn [acc [amp k w dx dz]]
                                 (+ acc (* amp dt
                                           (Math/sin (- (* k (+ (* dx x)
                                                                (* dz z)))
                                                        (* w t))))))
                               0.0 SWELL-TRAINS)]
              (-> p
                  (update :vy + lift)
                  (update :omega + (* CHOP-AMP dt (chop-rand x z kb))))))
          ps)))
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
  "Ocean from particle specs ({:x :z :omega ...} with defaults). bounds is
  the reflecting domain half-width; positions clamp into it so every particle
  lives inside the FMM domain (the sea is a bounded arena)."
  ([specs] (make-ocean specs BOUNDS))
  ([specs bounds]
   {:particles (mapv (fn [s]
                       (let [p (merge {:x 0.0 :y 0.0 :z 0.0
                                       :vx 0.0 :vy 0.0 :vz 0.0 :omega 0.0}
                                      s)
                             cl (fn [v] (max (- bounds) (min bounds v)))]
                         (assoc p :x (cl (:x p)) :z (cl (:z p)))))
                     specs)
    :bounds bounds
    :viscosity VISCOSITY
    :ambient true
    :p EXPANSION-P}))

(def sim-kernel
  "The native particle sim (voxel.seac), wired by voxel.physics/init! when
  the C library loads. A map of
  {:init! (fn [cols extent bounds ambient? viscosity sparse-max])
   :step! (fn [dt blasts hulls]) :particles (fn []) :time (fn [])
   :height (fn [x z]) :recenter! (fn [x z]) :origin (fn [])}.

  When it is present the game's particle state lives in flat native arrays
  and never crosses the FFI boundary per particle; the pure model in this
  namespace stays the definition of what the ocean is, and ocean-test holds
  the kernel to it step for step. With no kernel wired - tests, tooling -
  everything below runs in Clojure and behaves the same, only slower."
  (atom nil))

(defn lattice
  "The still particle sheet the sim lays out: cols x cols particles evenly
  covering [-extent, extent]^2, one per tile. Same layout as the C sim, so
  the pure fallback and the native path start from identical water."
  [cols extent]
  (let [sp (/ (* 2.0 extent) (dec (double cols)))]
    (vec (for [i (range cols)
               j (range cols)]
           {:x (+ (- extent) (* i sp)) :z (+ (- extent) (* j sp))
            :omega 0.0}))))

(defn make-sea
  "The game's ocean over [-extent, extent]^2, reflecting at +/- bounds:
  native when the sim kernel is wired, the pure model otherwise. Both step
  through step-ocean and are read through `particles`."
  [cols extent bounds]
  (if-let [k @sim-kernel]
    (do ((:init! k) cols extent bounds true VISCOSITY SPARSE-MAX)
        {:native true :cols cols :extent extent :bounds bounds
         :ambient true :viscosity VISCOSITY :p EXPANSION-P :time 0.0})
    (-> (make-ocean (lattice cols extent) bounds)
        (assoc :extent extent :cols cols))))

(defn particles
  "The ocean's particles as maps, whichever side they live on. Off the hot
  path: the renderer's mesh is filled inside C from the same arrays."
  [oc]
  (if (:native oc)
    ((:particles @sim-kernel))
    (:particles oc)))

(defn surface-height
  "Height of the water surface at (x, z): bilinear over the particle
  lattice, so it is the simulated surface and not a nominal y = 0. Oceans
  with no lattice (ad-hoc test clouds) read as flat."
  [oc x z]
  (if (:native oc)
    ((:height @sim-kernel) x z)
    (let [cols (:cols oc)
          extent (:extent oc)
          ps (:particles oc)]
      (if (or (nil? cols) (nil? extent) (< cols 2))
        0.0
        (let [sp (/ (* 2.0 extent) (dec (double cols)))
              top (- (dec (double cols)) 1e-9)
              cl (fn [v] (max 0.0 (min top v)))
              fi (cl (/ (+ x extent) sp))
              fj (cl (/ (+ z extent) sp))
              i (int fi) j (int fj)
              u (- fi i) v (- fj j)
              h (fn [a b] (:y (nth ps (+ (* a cols) b)) 0.0))]
          (+ (* (- 1.0 u) (- 1.0 v) (h i j))
             (* u (- 1.0 v) (h (inc i) j))
             (* (- 1.0 u) v (h i (inc j)))
             (* u v (h (inc i) (inc j)))))))))

(defn recenter!
  "Slide the simulated window of water so it covers what the camera is
  looking at. The window is a fixed lattice that scrolls: water leaving the
  trailing edge comes back as still water at the leading one, so the ocean
  reads as endless without simulating an endless amount of it.

  Only the native sim windows - the pure model is a fixed patch, and is the
  reference for what a step DOES, not for how much sea is kept."
  [oc x z]
  (when (:native oc)
    ((:recenter! @sim-kernel) x z))
  oc)

(defn origin
  "Where the centre of the simulated window sits in the world, [x z]."
  [oc]
  (if (:native oc)
    ((:origin @sim-kernel))
    [0.0 0.0]))

(defn surface-fn
  "A closure reading this ocean's surface height, so voxel.physics can float
  hulls on the water without knowing which side the particles live on."
  [oc]
  (fn [x z] (surface-height oc x z)))

(defn total-circulation
  [oc]
  (reduce + (map :omega (particles oc))))

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
  "Hulls in the water, as [{:x :z :r :hull-r :push :lift :swirl :displace
  :hx :hz}]. Every term is a rate, applied over dt, so the coupling does not
  get stronger just because the frame rate went up.

  Four things happen where a hull sits:

  - Static displacement. The hull occupies this water whether or not it is
    moving, so the surface sits down over its footprint and rises in a ring
    around it. The profile (s^2 - 1) e^(-s^2), s = d / hull-r, integrates to
    exactly zero over the plane, so the water pushed down comes back up
    somewhere - it is displaced, not deleted. `displace` is the hull's mean
    draft, which is its divergence-theorem displaced volume over its
    footprint, so a flooding wreck sits deeper and a shot-up one sits higher.

  - Displacement flow. Water has to get out of a moving hull's way and close
    in behind it: the 2D doublet 2(h.n)n - h of a body moving along h, which
    pushes water forward off the bow, aft along the beam, and forward again
    into the space behind the stern.

  - The bow and stern wave: water piled up ahead of her and drawn down
    astern, signed by h.n, so it is antisymmetric and moves no net volume.

  - Shed vorticity, signed by which side of her TRACK the water is on - the
    y component of heading x offset - so port and starboard shed opposite
    swirl and the wake follows her round. (Signing it by (nz - nx) instead,
    as this did, sheds on a fixed world diagonal whatever course she steers.)"
  [ps hulls dt]
  (if (empty? hulls)
    ps
    (mapv (fn [p]
            (reduce (fn [p {:keys [x z r push lift swirl displace hx hz]
                            :as hull}]
                      (let [dx (- (:x p) x)
                            dz (- (:z p) z)
                            d2 (+ (* dx dx) (* dz dz))
                            d (Math/sqrt d2)]
                        (if (>= d r)
                          p
                          (let [r0 (let [h (or (:hull-r hull) 0.0)]
                                     (if (pos? h) h (/ r 3.0)))
                                taper (- 1.0 (/ d r))
                                near (/ (* r0 r0) (+ d2 (* r0 r0)))
                                dd (max d 1e-6)
                                nx (/ dx dd)
                                nz (/ dz dd)
                                ;; no heading means no way on: she still
                                ;; displaces water, but leaves no wake
                                hx (or hx 0.0)
                                hz (or hz 0.0)
                                hn (+ (* hx nx) (* hz nz))
                                s2 (/ d2 (* r0 r0))
                                prof (* (- s2 1.0) (Math/exp (- s2)))
                                flow (* (or push 0.0) near taper)]
                            (-> p
                                (update :vx + (* flow (- (* 2.0 hn nx) hx) dt))
                                (update :vz + (* flow (- (* 2.0 hn nz) hz) dt))
                                (update :vy + (* (+ (* (or displace 0.0) prof)
                                                    (* (or lift 0.0) near
                                                       taper hn))
                                                 dt))
                                (update :omega + (* (or swirl 0.0) near taper
                                                    (- (* hx nz) (* hz nx))
                                                    dt)))))))
                    p hulls))
          ps)))

(def SURFACE-BREAK 0.7)  ; above this a particle is airborne spray, not surface
(def SURFACE-K 2.5)      ; surface restoring spring: the swell rides it
(def SURFACE-C 3.0)      ; surface damping: nearly critical, splashes settle in ~a second

(defn- step-vertical
  "Airborne spray (y > SURFACE-BREAK) falls ballistically under SPRAY-G.
  Surface water is a damped oscillator about y = 0 - the ambient swell
  rides it into travelling waves, blasts splash onto it and settle."
  [p dt]
  (if (> (:y p) SURFACE-BREAK)
    (let [y (+ (:y p) (* (:vy p) dt))
          vy (- (:vy p) (* SPRAY-G dt))]
      (if (< y SURFACE-BREAK)
        (assoc p :y SURFACE-BREAK :vy (* 0.1 vy)) ; the splash eats the plunge
        (assoc p :y y :vy vy)))
    (let [y (max -0.6 (+ (:y p) (* (:vy p) dt)))
          vy (- (:vy p) (* dt (+ (* SURFACE-K y)
                                 (* SURFACE-C (:vy p)))))]
      (assoc p :y y :vy vy))))

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

(def fmm-kernel
  "Optional accelerator for the full-churn FMM field: physics/init! resets
  this to the C kernel (voxel.seac/fmm) when the native library loads.
  Ambient turbulence keeps the whole sea mildly active, so the live game
  rides this path; tests run the pure FMM."

  (atom nil))

(defn velocities
  "The velocity field however it is cheapest at this sea state:
  - a still sea: no vortices, no field, no pair sums at all;
  - a scattering of live vortices: exact sums from the actives out to
    FIELD-RADIUS, still water beyond;
  - actives past SPARSE-MAX: the FMM, whose quadtree groups the whole sea
    so the cost stays linear in the particle count.

  direct-velocities is deliberately NOT in this dispatch - it is the O(N^2)
  oracle the other two are tested against. (It used to sit between the two
  branches below, where na > SPARSE-MAX >= n > DIRECT-MAX can never hold,
  so it was unreachable.)"
  [oc]
  (let [ps (:particles oc)
        n (count ps)
        na (count (filter #(> (Math/abs (:omega %)) ACTIVE-EPS) ps))]
    (cond
      (zero? na) (vec (repeat n [0.0 0.0 0.0]))
      (<= na SPARSE-MAX) (if-let [k @field-kernel]
                           (k oc)
                           (sparse-velocities ps))
      :else (if-let [k @fmm-kernel]
              (k oc)
              (fmm-velocities oc)))))

(defn- step-pure
  [oc dt blasts hulls]
   (let [ps (-> (:particles oc)
                 (apply-blasts blasts)
                 (apply-hulls hulls dt)
                 ((fn [ps] (if (:ambient oc)
                             (apply-ambient ps (or (:time oc) 0.0) dt)
                             ps))))
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
     (assoc oc :particles ps' :time (+ (or (:time oc) 0.0) dt))))

(defn step-ocean
  "Advance the ocean dt seconds. blasts and hulls are this frame's couplings
  from the world (see apply-blasts / apply-hulls). A native ocean steps
  inside the kernel; everything below is the pure model it mirrors."
  ([oc dt] (step-ocean oc dt nil nil))
  ([oc dt blasts] (step-ocean oc dt blasts nil))
  ([oc dt blasts hulls]
   (if (:native oc)
     (let [k @sim-kernel]
       ((:step! k) dt blasts hulls)
       (assoc oc :time ((:time k))))
     (step-pure oc dt blasts hulls))))
