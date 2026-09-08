(ns voxel.seac
  "Sea kernels in C (native/voxel_sea.c, built by `jolt sea`).

  voxel.ocean/sparse-velocities is the pure reference; vsea_field is the
  same sum (same order over actives, same truncation) at native speed, so a
  whole-sea field costs microseconds instead of tens of milliseconds.

  vsea_mesh_* builds and draws the entire wave sheet as ONE raylib mesh in
  C - issuing rlVertex3f per corner from jolt costs microseconds a call,
  which is tens of milliseconds for a full sea. Dense churn still goes to
  the FMM; this kernel covers the sparse regime.

  The frame loop is single-threaded, so scratch buffers are reused."
  (:require [jolt.ffi :as ffi]))

(ffi/defcfn field* "vsea_field"
  [:pointer :pointer :pointer :pointer :int64 :double :double
   :pointer :pointer :int64] :void)

(ffi/defcfn fmm* "vsea_fmm"
  [:pointer :pointer :pointer :int64 :int64 :double :pointer :pointer] :void)

(ffi/defcfn mesh-init* "vsea_mesh_init" [] :void)
(ffi/defcfn mesh-update* "vsea_mesh_update"
  [:pointer :pointer :pointer :pointer] :void)
(ffi/defcfn mesh-draw* "vsea_mesh_draw" [] :void)
(ffi/defcfn mesh-free* "vsea_mesh_free" [] :void)
(ffi/defcfn mesh-vertex-count* "vsea_mesh_vertex_count" [] :int64)
(ffi/defcfn mesh-read* "vsea_mesh_read" [:pointer :pointer :pointer] :void)

(def ^:private bufs
  "Field scratch [capacity xs zs om act vx vz], grown on demand."
  (atom nil))

(defn- ensure-buffers!
  [n]
  (when (or (nil? @bufs) (> n (first @bufs)))
    (let [cap (max n 4096)]
      (reset! bufs (into [cap] (repeatedly 6 #(ffi/alloc (* 8 cap))))))))

(defn- write-color!
  "Packed little-endian rgba int -> four bytes in buf."
  [buf packed]
  (dotimes [k 4]
    (ffi/write buf :uint8 (bit-and 0xFF (bit-shift-right packed (* 8 k))) k)))

(def ^:private mesh-bufs
  "Sheet-mesh scratch: the lighting directions and base colours. The vertex
  data never crosses the boundary at all - C fills the mesh straight from
  the particle arrays it already owns."
  (atom nil))

(defn- ensure-mesh-buffers!
  []
  (when (nil? @mesh-bufs)
    (reset! mesh-bufs {:sun (ffi/alloc 24)
                       :half (ffi/alloc 24)
                       :swell (ffi/alloc 4)
                       :foam (ffi/alloc 4)})))

(defn mesh-vertex-count [] (mesh-vertex-count*))

(defn mesh-init!
  "Build the sheet mesh over the live particle sim: one vertex per particle."
  []
  (mesh-init*))

(defn mesh-update!
  "Refill and upload the sheet from the live particle state. sun/half are the
  lighting directions (3-vectors), swell/foam the packed base colours."
  [sun half swell foam]
  (ensure-mesh-buffers!)
  (let [b @mesh-bufs]
    (dotimes [k 3]
      (ffi/write (:sun b) :double (double (nth sun k)) (* 8 k))
      (ffi/write (:half b) :double (double (nth half k)) (* 8 k)))
    (write-color! (:swell b) swell)
    (write-color! (:foam b) foam)
    (mesh-update* (:sun b) (:half b) (:swell b) (:foam b))))

(defn mesh-draw!
  "One draw call for the whole sheet, identity transform."
  []
  (mesh-draw*))

(defn mesh-free!
  "Release the sheet mesh."
  []
  (mesh-free*))

(ffi/defcfn ship-init* "vsea_ship_init" [:pointer :pointer :int64] :int64)
(ffi/defcfn ship-draw* "vsea_ship_draw"
  [:int64 :pointer :pointer :pointer :pointer :pointer] :void)
(ffi/defcfn ship-free* "vsea_ship_free" [:int64] :void)
(ffi/defcfn ship-vertex-count* "vsea_ship_vertex_count" [:int64] :int64)
(ffi/defcfn ship-read* "vsea_ship_read" [:int64 :pointer :pointer :pointer] :void)

(defn- dir-index [d]
  (case d
    ([1 0 0]) 0
    ([-1 0 0]) 1
    ([0 1 0]) 2
    ([0 -1 0]) 3
    ([0 0 1]) 4
    ([0 0 -1]) 5
    -1))

(def ^:private ship-bufs (atom nil))

(defn- ensure-ship-buffers!
  [n]
  (when (or (nil? @ship-bufs) (> n (:cap @ship-bufs)))
    (reset! ship-bufs {:cap (max n 1024)
                       :cells (ffi/alloc (* 32 (max n 1024)))
                       :colors (ffi/alloc (* 4 (max n 1024)))
                       :v3 (ffi/alloc 24)
                       :q (ffi/alloc 32)
                       :v3b (ffi/alloc 24)
                       :v3c (ffi/alloc 24)
                       :v3d (ffi/alloc 24)})))

(defn- write-color!-at
  [buf packed off]
  (dotimes [k 4]
    (ffi/write buf :uint8 (bit-and 0xFF (bit-shift-right packed (* 8 k)))
               (+ off k))))

(defn ship-init!
  "Build a hull's static mesh from its exposed faces (seq of [[i j k] dir])
  with a packed base colour per face. Returns the hull id (or -1)."
  [faces colors]
  (let [n (count faces)]
    (ensure-ship-buffers! n)
    (let [b @ship-bufs]
      (dotimes [f n]
        (let [[cell dir] (nth faces f)
              [i j k] cell]
          (ffi/write (:cells b) :int64 (long i) (* 32 f))
          (ffi/write (:cells b) :int64 (long j) (+ 8 (* 32 f)))
          (ffi/write (:cells b) :int64 (long k) (+ 16 (* 32 f)))
          (ffi/write (:cells b) :int64 (long (dir-index dir)) (+ 24 (* 32 f))))
        (write-color!-at (:colors b) (nth colors f) (* 4 f)))
      (ship-init* (:cells b) (:colors b) (long n)))))

(defn ship-draw!
  "Draw hull id at its physics transform: pos/quat/anchor are the ship's,
  sun and cam the lighting/view directions. C culls, shades and submits."
  [id pos quat anchor sun cam]
  (let [b @ship-bufs]
    (dotimes [k 3]
      (ffi/write (:v3 b) :double (double (nth pos k)) (* 8 k))
      (ffi/write (:v3b b) :double (double (nth anchor k)) (* 8 k))
      (ffi/write (:v3c b) :double (double (nth sun k)) (* 8 k))
      (ffi/write (:v3d b) :double (double (nth cam k)) (* 8 k)))
    (dotimes [k 4]
      (ffi/write (:q b) :double (double (nth quat k)) (* 8 k)))
    (ship-draw* (long id) (:v3 b) (:q b) (:v3b b) (:v3c b) (:v3d b))))

(defn ship-free!
  [id]
  (ship-free* (long id)))

(ffi/defcfn sim-init* "vsea_sim_init"
  [:int :double :double :int :double :int] :void)
(ffi/defcfn sim-free* "vsea_sim_free" [] :void)
(ffi/defcfn sim-count* "vsea_sim_count" [] :int64)
(ffi/defcfn sim-cols* "vsea_sim_cols" [] :int64)
(ffi/defcfn sim-spacing* "vsea_sim_spacing" [] :double)
(ffi/defcfn sim-extent* "vsea_sim_extent" [] :double)
(ffi/defcfn sim-time* "vsea_sim_time" [] :double)
(ffi/defcfn sim-circulation* "vsea_sim_circulation" [] :double)
(ffi/defcfn sim-height* "vsea_sim_height" [:double :double] :double)
(ffi/defcfn sim-step* "vsea_sim_step"
  [:double :pointer :int64 :pointer :int64] :void)
(ffi/defcfn sim-load* "vsea_sim_load"
  [:pointer :pointer :pointer :pointer :pointer :pointer :pointer
   :int64 :double] :void)
(ffi/defcfn sim-read* "vsea_sim_read"
  [:pointer :pointer :pointer :pointer :pointer :pointer :pointer] :void)

(def ^:private sim-bufs
  "Scratch for the sim boundary: seven state columns plus the coupling
  packs. Allocated once and reused - the frame loop is single-threaded, and
  nothing here is on the per-particle hot path anyway."
  (atom nil))

(defn- ensure-sim-buffers!
  [n]
  (when (or (nil? @sim-bufs) (> n (:cap @sim-bufs)))
    (let [cap (max n 4096)]
      (reset! sim-bufs
              (into {:cap cap
                     :blasts (ffi/alloc (* 8 4 64))
                     :hulls (ffi/alloc (* 8 7 64))}
                    (map (fn [k] [k (ffi/alloc (* 8 cap))]))
                    [:x :z :y :vx :vy :vz :om])))))

(def ^:private SIM-COLS [:x :z :y :vx :vy :vz :om])

(defn sim-init!
  "Lay out a still cols x cols particle sheet covering [-extent, extent]^2,
  reflecting at +/- bounds. Returns the particle count."
  [cols extent bounds ambient? viscosity sparse-max]
  (sim-init* (int cols) (double extent) (double bounds)
             (int (if ambient? 1 0)) (double viscosity) (int sparse-max))
  (ensure-sim-buffers! (max 1 (sim-count*)))
  (sim-count*))

(defn sim-free! [] (sim-free*))
(defn sim-count [] (sim-count*))
(defn sim-cols [] (sim-cols*))
(defn sim-spacing [] (sim-spacing*))
(defn sim-extent [] (sim-extent*))
(defn sim-time [] (sim-time*))
(defn sim-circulation [] (sim-circulation*))

(defn sim-height
  "Water surface height at (x, z), bilinear over the particle lattice."
  [x z]
  (sim-height* (double x) (double z)))

(def ^:private MAX-COUPLINGS 64)

(defn- write-pack!
  "Pack maps into a flat double buffer, ks fields each, capped."
  [buf items ks]
  (let [items (vec (take MAX-COUPLINGS items))
        w (count ks)]
    (dotimes [i (count items)]
      (let [m (nth items i)]
        (dotimes [f w]
          (ffi/write buf :double (double (or (get m (nth ks f)) 0.0))
                     (* 8 (+ (* i w) f))))))
    (count items)))

(defn sim-step!
  "Advance the native ocean dt seconds under this frame's couplings.
  blasts are {:x :z :r :power}, hulls {:x :z :r :push :swirl :hx :hz}."
  [dt blasts hulls]
  (let [b @sim-bufs
        nb (write-pack! (:blasts b) blasts [:x :z :r :power])
        nh (write-pack! (:hulls b) hulls [:x :z :r :push :swirl :hx :hz])]
    (sim-step* (double dt) (:blasts b) (long nb) (:hulls b) (long nh))))

(defn sim-particles
  "The native particle state as ocean-shaped maps. Off the hot path - the
  renderer reads the same arrays inside C - so this is for tests, the REPL
  and anything that wants the pure representation back."
  []
  (let [n (sim-count*)]
    (if (zero? n)
      []
      (let [b @sim-bufs
            cols (mapv #(get b %) SIM-COLS)]
        (apply sim-read* cols)
        (mapv (fn [i]
                (let [g (fn [c] (ffi/read c :double (* 8 i)))]
                  {:x (g (cols 0)) :z (g (cols 1)) :y (g (cols 2))
                   :vx (g (cols 3)) :vy (g (cols 4)) :vz (g (cols 5))
                   :omega (g (cols 6))}))
              (range n))))))

(defn sim-load!
  "Overwrite the native particle state from ocean-shaped maps. Tests use
  this to put the C sim and the pure reference on identical footing."
  [ps t]
  (let [b @sim-bufs
        cols (mapv #(get b %) SIM-COLS)
        ks [:x :z :y :vx :vy :vz :omega]]
    (dotimes [i (count ps)]
      (let [p (nth ps i)]
        (dotimes [f 7]
          (ffi/write (cols f) :double (double (or (get p (nth ks f)) 0.0))
                     (* 8 i)))))
    (apply sim-load* (conj cols (long (count ps)) (double t)))))

;; --- headless inspection -----------------------------------------------------
;;
;; vsea_mesh_init / vsea_ship_init skip every GL call when no window is up,
;; so the CPU-side vertex arrays are filled either way. These read them back,
;; which is how the sheet and the hulls are unit-tested: exactly the bytes the
;; GPU would have been handed, no window, no screenshot diffing.

(defn- read-buffers
  "[verts norms colors] from a filled CPU mesh: n vertices, positions and
  normals as [x y z] doubles, colours as [r g b a] ints."
  [n fill!]
  (if (zero? n)
    [[] [] []]
    (let [vb (ffi/alloc (* 8 3 n))
          nb (ffi/alloc (* 8 3 n))
          cb (ffi/alloc (* 4 n))]
      (fill! vb nb cb)
      [(mapv (fn [i] (mapv #(ffi/read vb :double (* 8 (+ (* 3 i) %))) (range 3)))
             (range n))
       (mapv (fn [i] (mapv #(ffi/read nb :double (* 8 (+ (* 3 i) %))) (range 3)))
             (range n))
       (mapv (fn [i] (mapv #(ffi/read cb :uint8 (+ (* 4 i) %)) (range 4)))
             (range n))])))

(defn ship-buffers
  "[verts norms colors] of hull id as the GPU would receive them."
  [id]
  (read-buffers (ship-vertex-count* (long id))
                (fn [vb nb cb] (ship-read* (long id) vb nb cb))))

(defn mesh-buffers
  "[verts norms colors] of the sea sheet as the GPU would receive them."
  []
  (read-buffers (mesh-vertex-count*) mesh-read*))

(defn field
  "Particles -> per-particle [fx 0.0 fz], the reference sparse field at C
  speed. r truncates the field (water beyond it rests), eps is the
  still-water vorticity floor."
  [ps r eps]
  (let [n (count ps)]
    (ensure-buffers! n)
    (let [[_cap xs zs om act vx vz] @bufs
          act-idx (vec (keep-indexed (fn [i p]
                                       (when (> (Math/abs (:omega p)) eps) i))
                                     ps))]
      (dotimes [i n]
        (let [p (nth ps i)]
          (ffi/write xs :double (double (:x p)) (* 8 i))
          (ffi/write zs :double (double (:z p)) (* 8 i))
          (ffi/write om :double (double (:omega p)) (* 8 i))))
      (dotimes [i (count act-idx)]
        (ffi/write act :int64 (long (act-idx i)) (* 8 i)))
       (field* xs zs om act (long (count act-idx))
               (double (* r r)) (double eps) vx vz (long n))
       (mapv (fn [i]
               [(ffi/read vx :double (* 8 i))
                0.0
                (ffi/read vz :double (* 8 i))])
             (range n)))))

(defn fmm
  "Particles -> per-particle [fx 0.0 fz] via the C multi-level quadtree
  FMM: the same field as voxel.ocean/fmm-velocities (same tree, same
  expansion conventions) at native speed, for fully-churned seas where
  every particle is active."
  [ps p bounds]
  (let [n (count ps)]
    (ensure-buffers! n)
    (let [[_cap xs zs om _act vx vz] @bufs]
      (dotimes [i n]
        (let [pc (nth ps i)]
          (ffi/write xs :double (double (:x pc)) (* 8 i))
          (ffi/write zs :double (double (:z pc)) (* 8 i))
          (ffi/write om :double (double (:omega pc)) (* 8 i))))
      (fmm* xs zs om (long n) (long p) (double bounds) vx vz)
      (mapv (fn [i]
              [(ffi/read vx :double (* 8 i))
               0.0
               (ffi/read vz :double (* 8 i))])
            (range n)))))
