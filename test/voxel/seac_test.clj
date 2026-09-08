(ns voxel.seac-test
  "The C render kernels, headless. vsea_*_init skip every GL call when no
  window is up but still fill the CPU vertex arrays, so these tests read back
  exactly the bytes the GPU would have been handed - no window, no screenshot
  diffing."
  (:require [clojure.test :refer [deftest is testing]]
            [voxel.buoyancy :as buoy]
            [voxel.render :as render]
            [voxel.seac :as seac]
            [voxel.ship :as ship]))

(defn- cube-faces
  "Exposed faces of a solid cube of cells, as ship-init! wants them."
  [n]
  (let [cells (zipmap (for [i (range n) j (range n) k (range n)] [i j k])
                      (repeat :hull))]
    (vec (buoy/surface-faces cells))))

(defn- with-hull
  "Build a hull mesh from faces, run f with its id, always free it."
  [faces f]
  (let [colors (vec (repeat (count faces) (unchecked-int 0xFF806040)))
        id (seac/ship-init! faces colors)]
    (is (>= id 0) "hull mesh allocated")
    (try (f id)
         (finally (seac/ship-free! id)))))

(def ^:private SUN [-0.45 0.78 0.35])

;; --- culled faces must still be written ------------------------------------
;;
;; Every face owns six slots in a static index buffer, so a face the camera
;; cannot see is still drawn. Leaving its slots unwritten hands the GPU
;; whatever was there: malloc garbage on the first frame, the pose from
;; whenever the face was last visible afterwards. Both render as huge stray
;; triangles across the arena.

(deftest every-face-is-written-on-the-first-frame
  (testing "no vertex is left at whatever the allocator happened to hold"
    (let [faces (cube-faces 2)]
      (with-hull faces
        (fn [id]
          (seac/ship-draw! id [4.0 -1.0 7.0] [0.0 0.0 0.0 1.0] [1.0 1.0 1.0]
                           SUN [-60.0 62.0 -60.0])
          (let [[verts _ _] (seac/ship-buffers id)]
            (is (= (* 6 (count faces)) (count verts)) "six vertices per face")
            ;; NaN and Inf both fail this comparison, so it is the finite
            ;; check as well as the proximity one
            (is (every? (fn [v] (every? #(< (Math/abs %) 100.0) v)) verts)
                "every vertex is finite and near the hull")))))))

(deftest culled-faces-collapse-instead-of-going-stale
  (testing "a face visible at one pose leaves no geometry behind at the next"
    (let [faces (cube-faces 2)
          far [80.0 0.0 80.0]]
      (with-hull faces
        (fn [id]
          ;; frame 1: hull at the origin, roughly half its faces visible
          (seac/ship-draw! id [0.0 0.0 0.0] [0.0 0.0 0.0 1.0] [1.0 1.0 1.0]
                           SUN [-60.0 62.0 -60.0])
          ;; frame 2: same hull far away. Faces culled here would otherwise
          ;; still be sitting at the frame-1 pose, near the origin.
          (seac/ship-draw! id far [0.0 0.0 0.0 1.0] [1.0 1.0 1.0]
                           SUN [-60.0 62.0 -60.0])
          (let [[verts _ _] (seac/ship-buffers id)]
            (is (every? (fn [[x _ z]]
                          (and (< (Math/abs (- x (far 0))) 8.0)
                               (< (Math/abs (- z (far 2))) 8.0)))
                        verts)
                "every vertex, culled or drawn, sits at the current pose")))))))

(deftest culled-faces-are-zero-area
  (testing "collapsed faces are degenerate triangles, not visible geometry"
    (let [faces (cube-faces 2)]
      (with-hull faces
        (fn [id]
          (seac/ship-draw! id [0.0 0.0 0.0] [0.0 0.0 0.0 1.0] [1.0 1.0 1.0]
                           SUN [-60.0 62.0 -60.0])
          (let [[verts _ _] (seac/ship-buffers id)
                area (fn [[a b c]]
                       (let [u (mapv - b a) v (mapv - c a)
                             cx (- (* (u 1) (v 2)) (* (u 2) (v 1)))
                             cy (- (* (u 2) (v 0)) (* (u 0) (v 2)))
                             cz (- (* (u 0) (v 1)) (* (u 1) (v 0)))]
                         (* 0.5 (Math/sqrt (+ (* cx cx) (* cy cy) (* cz cz))))))
                tris (partition 3 verts)
                drawn (remove #(< (area %) 1e-12) tris)]
            (is (pos? (count drawn)) "the camera-facing faces still draw")
            (is (< (count drawn) (count tris))
                "the away-facing faces were collapsed, not drawn")
            (is (every? #(< (Math/abs (- (area %) 0.5)) 1e-5) drawn)
                "every drawn triangle is half a unit quad")))))))

;; --- shading is rgb, never alpha -------------------------------------------

(deftest hull-shading-leaves-alpha-alone
  (testing "a hull face in shadow darkens but does not turn translucent"
    (let [faces (cube-faces 2)]
      (with-hull faces
        (fn [id]
          (seac/ship-draw! id [0.0 0.0 0.0] [0.0 0.0 0.0 1.0] [1.0 1.0 1.0]
                           SUN [-60.0 62.0 -60.0])
          (let [[verts _ colors] (seac/ship-buffers id)
                lit (keep-indexed (fn [i c]
                                    ;; collapsed faces are cleared to zero
                                    (when-not (= [0 0 0 0] c)
                                      [(nth verts i) c]))
                                  colors)]
            (is (pos? (count lit)) "some faces drew")
            (is (every? (fn [[_ c]] (= 255 (c 3))) lit)
                "alpha stays opaque whatever the sun angle")))))))

(defn- sea-particles
  "A small still sheet: one particle per tile of a cols x cols lattice."
  [cols spacing extent]
  (vec (for [i (range cols)
             j (range cols)]
         {:x (+ (- extent) (* i spacing))
          :z (+ (- extent) (* j spacing))
          :y 0.0 :omega 0.0})))

(deftest sea-shading-leaves-alpha-alone
  (testing "shaded water stays opaque - the sky must not show through troughs"
    (let [cols 8 spacing 1.5 extent 6.0]
      (seac/mesh-init! cols cols spacing extent)
      (seac/mesh-update! (sea-particles cols spacing extent) 0.0
                         SUN [0.0 1.0 0.0] render/DEEP render/SWELL render/FOAM)
      (let [[_ _ colors] (seac/mesh-buffers)]
        (is (pos? (count colors)))
        (is (every? #(= 255 (% 3)) colors)
            "every sea vertex is fully opaque")))))

;; --- shadow footprint ------------------------------------------------------

(deftest ship-extent-is-measured-about-the-anchor
  (testing "the dreadnought's shadow is its real footprint, not its grid offset"
    (let [layout (ship/dreadnought)
          faces (vec (buoy/surface-faces (:cells layout)))
          [ex ez] (render/ship-extent faces (:anchor layout))]
      ;; hull is 7 wide (i 0..6) and 26 long (k 0..25) about anchor [3 0 13]
      (is (< (Math/abs (- ex 4.0)) 0.51) (str "half-beam ~3.5, got " ex))
      (is (< (Math/abs (- ez 13.5)) 0.51) (str "half-length ~13, got " ez))))
  (testing "a cell at the anchor has a half-cell footprint"
    (is (= [1.0 1.0] (render/ship-extent [[[3 0 13] [0 1 0]]] [3.0 0.0 13.0])))))
