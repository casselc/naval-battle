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

(ffi/defcfn mesh-init* "vsea_mesh_init" [:int :int :double :double] :void)
(ffi/defcfn mesh-update* "vsea_mesh_update"
  [:pointer :pointer :pointer :pointer :int64 :double
   :pointer :pointer :pointer :pointer :pointer] :void)
(ffi/defcfn mesh-draw* "vsea_mesh_draw" [] :void)
(ffi/defcfn mesh-free* "vsea_mesh_free" [] :void)

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
  "Sheet-mesh scratch: {:cap :ys :om :sun :half :deep :swell :foam}."
  (atom nil))

(defn- ensure-mesh-buffers!
  [n]
  (when (or (nil? @mesh-bufs) (> n (:cap @mesh-bufs)))
    (reset! mesh-bufs {:cap (max n 4096)
                       :ys (ffi/alloc (* 8 (max n 4096)))
                       :xs (ffi/alloc (* 8 (max n 4096)))
                       :zs (ffi/alloc (* 8 (max n 4096)))
                       :om (ffi/alloc (* 8 (max n 4096)))
                       :sun (ffi/alloc 24)
                       :half (ffi/alloc 24)
                       :deep (ffi/alloc 4)
                       :swell (ffi/alloc 4)
                       :foam (ffi/alloc 4)})))

(defn mesh-init!
  "Build the sheet mesh for a cols x rows particle lattice with the given
  tile spacing and square extent."
  [cols rows spacing extent]
  (mesh-init* (int cols) (int rows) (double spacing) (double extent)))

(defn mesh-update!
  "Refill and upload the sheet mesh from per-particle spray heights and
  vorticities. sun/half are the lighting directions (3-vectors), and
  deep/swell/foam the packed base colors."
  [ps t sun half deep swell foam]
  (ensure-mesh-buffers! (count ps))
  (let [b @mesh-bufs
        ys (:ys b)
        xs (:xs b)
        zs (:zs b)
        om (:om b)
        sunb (:sun b)
        halfb (:half b)
        deepb (:deep b)
        swellb (:swell b)
        foamb (:foam b)]
    (dotimes [i (count ps)]
      (let [p (nth ps i)]
        (ffi/write ys :double (double (or (:y p) 0.0)) (* 8 i))
        (ffi/write xs :double (double (or (:x p) 0.0)) (* 8 i))
        (ffi/write zs :double (double (or (:z p) 0.0)) (* 8 i))
        (ffi/write om :double (double (or (:omega p) 0.0)) (* 8 i))))
    (dotimes [k 3]
      (ffi/write sunb :double (double (nth sun k)) (* 8 k))
      (ffi/write halfb :double (double (nth half k)) (* 8 k)))
    (write-color! deepb deep)
    (write-color! swellb swell)
    (write-color! foamb foam)
    (mesh-update* ys xs zs om (long (count ps)) (double t)
                  sunb halfb deepb swellb foamb)))

(defn mesh-draw!
  "One draw call for the whole sheet, identity transform."
  []
  (mesh-draw*))

(ffi/defcfn ship-init* "vsea_ship_init" [:pointer :pointer :int64] :int64)
(ffi/defcfn ship-draw* "vsea_ship_draw"
  [:int64 :pointer :pointer :pointer :pointer :pointer] :void)
(ffi/defcfn ship-free* "vsea_ship_free" [:int64] :void)

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
