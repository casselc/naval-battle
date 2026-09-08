(ns voxel.hullc
  "Hull kernels in C (native/voxel_hull.c, built by `jolt hull`).

  voxel.buoyancy is the reference for every bit of this and
  voxel.buoyancy-test holds the two together. The divergence-theorem solve
  there is O(triangles) at about eleven flops each, but in Clojure it
  allocates several vectors per triangle, and that - not the arithmetic - is
  what caps how many voxels a ship can be made of: a 5000-triangle hull cost
  5.3ms a step against a flop count worth twenty microseconds.

  So a hull's surface mesh lives in C. Clojure hands over the cell list when
  damage changes it and asks for volume and centre of buoyancy each step;
  nothing per-triangle crosses the boundary."
  (:require [jolt.ffi :as ffi]))

(ffi/defcfn hull-set* "vsea_hull_set"
  [:int64 :pointer :int64 :pointer :double :pointer] :int64)
(ffi/defcfn hull-faces* "vsea_hull_faces" [:int64 :pointer] :int64)
(ffi/defcfn hull-free* "vsea_hull_free" [:int64] :void)
(ffi/defcfn hull-metrics* "vsea_hull_metrics"
  [:int64 :pointer :pointer :pointer :pointer] :void)

(def MAX-HULLS 64)

(def ^:private bufs
  "Per-call scratch, grown on demand. The frame loop is single-threaded and
  nothing here is per-triangle."
  (atom nil))

(defn- ensure!
  [cells]
  (let [need (max cells 4096)]
    (when (or (nil? @bufs) (> need (:cap @bufs)))
      (reset! bufs {:cap need
                    :cells (ffi/alloc (* 4 3 need))
                    :faces (ffi/alloc (* 4 4 need))
                    :out (ffi/alloc (* 8 8))
                    :pos (ffi/alloc 24)
                    :quat (ffi/alloc 32)
                    :plane (ffi/alloc 32)
                    :anchor (ffi/alloc 24)}))
    @bufs))

(defn- write-v3!
  [buf v]
  (dotimes [k 3] (ffi/write buf :double (double (nth v k)) (* 8 k))))

(def ^:private DIRS
  "Face direction by the index the kernel reports."
  [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn set-hull!
  "Give hull `id` its cell list. Rebuilds the exposed faces and the closed
  surface mesh in C, and returns
  {:faces [[cell dir] ...] :span [ex ey ez] :com [x y z] :tri-count n},
  all in world units - `voxel` is how many of those a cell edge is.

  Called on spawn and whenever damage changes the cells, never per step."
  [id cells anchor voxel]
  (let [cs (vec cells)
        n (count cs)
        b (ensure! (max n 1))]
    (dotimes [c n]
      (let [cell (nth cs c)]
        (dotimes [k 3]
          (ffi/write (:cells b) :int32 (int (nth cell k)) (* 4 (+ (* 3 c) k))))))
    (write-v3! (:anchor b) anchor)
    (let [nf (hull-set* (long id) (:cells b) (long n) (:anchor b)
                        (double voxel) (:out b))
          rd (fn [i] (ffi/read (:out b) :double (* 8 i)))]
      (when (neg? nf)
        (throw (ex-info "hull id or cell count out of range"
                        {:id id :cells n})))
      (hull-faces* (long id) (:faces b))
      {:tri-count (long (rd 0))
       :span [(rd 2) (rd 3) (rd 4)]
       :com [(rd 5) (rd 6) (rd 7)]
       :faces (mapv (fn [f]
                      (let [g (fn [k] (ffi/read (:faces b) :int32
                                                (* 4 (+ (* 4 f) k))))]
                        [[(g 0) (g 1) (g 2)] (DIRS (g 3))]))
                    (range nf))})))

(defn free-hull! [id] (hull-free* (long id)))

(defn metrics
  "{:volume v :centroid [x y z]} of hull `id` under the water plane
  [nx ny nz d] at pose pos/quat, or {:volume 0.0 :centroid nil} when dry."
  [id pos quat plane]
  (let [b (ensure! 1)]
    (write-v3! (:pos b) pos)
    (dotimes [k 4] (ffi/write (:quat b) :double (double (nth quat k)) (* 8 k)))
    (dotimes [k 4] (ffi/write (:plane b) :double (double (nth plane k)) (* 8 k)))
    (hull-metrics* (long id) (:pos b) (:quat b) (:plane b) (:out b))
    (let [rd (fn [i] (ffi/read (:out b) :double (* 8 i)))
          v (rd 0)]
      (if (zero? v)
        {:volume 0.0 :centroid nil}
        {:volume v :centroid [(rd 1) (rd 2) (rd 3)]}))))
