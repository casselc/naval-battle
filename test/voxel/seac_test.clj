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
        id (seac/ship-init! faces colors 1.0)]
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

(defn- with-sheet
  "A live native sim plus its mesh, torn down after f."
  [cols extent f]
  (seac/sim-init! cols extent (+ extent 4.0) true 0.04 2048)
  (seac/mesh-init!)
  (try (f) (finally (seac/mesh-free!) (seac/sim-free!))))

(deftest sea-shading-leaves-alpha-alone
  (testing "shaded water stays opaque - the sky must not show through troughs"
    (with-sheet 8 6.0
      (fn []
        (seac/mesh-update! SUN [0.0 1.0 0.0] render/DEEP render/SWELL render/FOAM)
        (let [[_ _ colors] (seac/mesh-buffers)]
          (is (pos? (count colors)))
          (is (every? #(= 255 (% 3)) colors)
              "every sea vertex is fully opaque"))))))

;; --- the sheet is the particle set ------------------------------------------

(deftest the-sheet-is-one-vertex-per-particle
  (testing "no interpolated corner lattice, no analytic ring: the drawn
            surface is exactly the simulated particles"
    (with-sheet 12 10.0
      (fn []
        (seac/mesh-update! SUN [0.0 1.0 0.0] render/DEEP render/SWELL render/FOAM)
        (let [[verts _ _] (seac/mesh-buffers)
              ps (seac/sim-particles)]
          (is (= (count ps) (count verts)))
          (is (= 144 (count verts)) "12 x 12 lattice")
          (doseq [i (range (count ps))]
            (let [p (nth ps i)
                  [x y z] (nth verts i)]
              (is (< (Math/abs (- x (:x p))) 1e-6) "vertex x is the particle's")
              (is (< (Math/abs (- z (:z p))) 1e-6) "vertex z is the particle's")
              ;; the sheet rides a hair above y so it clears the shadow quads
              (is (< (Math/abs (- y (+ (:y p) 0.05))) 1e-6)
                  "vertex height is the particle's height and nothing else"))))))))

(deftest the-sheet-height-follows-the-particles
  (testing "a still sea is flat; the height a player sees comes from the sim"
    (with-sheet 10 8.0
      (fn []
        (seac/mesh-update! SUN [0.0 1.0 0.0] render/DEEP render/SWELL render/FOAM)
        (let [[flat _ _] (seac/mesh-buffers)]
          (is (every? #(< (Math/abs (- (second %) 0.05)) 1e-9) flat)
              "nothing analytic is added to a sea at rest")
          ;; a blast throws water up; the sheet must rise with it
          (seac/sim-step! 0.016 [{:x 0.0 :z 0.0 :r 6.0 :power 9.0}] nil)
          (seac/mesh-update! SUN [0.0 1.0 0.0] render/DEEP render/SWELL render/FOAM)
          (let [[bumped _ _] (seac/mesh-buffers)]
            (is (> (apply max (map second bumped))
                   (+ 0.05 (apply max (map second flat))))
                "the blast shows in the drawn surface")))))))

;; --- shadow footprint ------------------------------------------------------

(deftest ship-extent-is-measured-about-the-anchor
  (testing "the dreadnought's shadow is its real footprint, not its grid offset"
    (let [layout (ship/dreadnought)
          faces (vec (buoy/surface-faces (:cells layout)))
          [ex ez] (render/ship-extent faces (:anchor layout) (:voxel layout))]
      ;; the answer is in world units whatever the grid resolution is
      (is (< (Math/abs (- ex (* 0.5 ship/BEAM-U))) 0.6)
          (str "half-beam ~" (* 0.5 ship/BEAM-U) ", got " ex))
      (is (< (Math/abs (- ez (* 0.5 ship/LENGTH-U))) 0.6)
          (str "half-length ~" (* 0.5 ship/LENGTH-U) ", got " ez))))
  (testing "a cell at the anchor has a half-cell footprint"
    (is (= [1.0 1.0] (render/ship-extent [[[3 0 13] [0 1 0]]] [3.0 0.0 13.0]))))
  (testing "and it scales with the voxel size, not the cell count"
    (is (= [0.5 0.5]
           (render/ship-extent [[[3 0 13] [0 1 0]]] [3.0 0.0 13.0] 0.5)))))


;; --- what the water looks like ----------------------------------------------

(defn- sheet-colors
  "Vertex colours of the sheet after seeding the sim with `setup`."
  [cols extent setup]
  (seac/sim-init! cols extent (+ extent 4.0) false 0.0 2048)
  (seac/mesh-init!)
  (setup)
  (seac/mesh-update! SUN [0.0 1.0 0.0] render/DEEP render/SWELL render/FOAM)
  (let [[_ _ colors] (seac/mesh-buffers)]
    (seac/mesh-free!)
    (seac/sim-free!)
    colors))

(defn- brightness [c] (+ (c 0) (c 1) (c 2)))

(deftest ordinary-swell-does-not-foam
  (testing "only water thrown clear of the swell, or genuinely churned, goes
            white - scaling foam off raw height whitens every crest and
            leaves nothing for a wake to stand out against"
    (let [swell (sheet-colors 12 10.0
                              (fn []
                                ;; a gentle sea: crests well inside freeboard
                                (seac/sim-load!
                                 (mapv (fn [p] (assoc p :y 0.25 :omega 0.004))
                                       (seac/sim-particles))
                                 0.0)))
          wake (sheet-colors 12 10.0
                             (fn []
                               (seac/sim-load!
                                (mapv (fn [p] (assoc p :y 0.25 :omega 4.0))
                                      (seac/sim-particles))
                                0.0)))]
      (is (< (apply max (map brightness swell))
             (apply min (map brightness wake)))
          "churned water is brighter than any part of an unbroken swell"))))

(deftest the-sea-is-coloured-by-where-the-water-stands
  (testing "troughs shade toward the deep colour, crests toward the swell one"
    (let [at (fn [y] (sheet-colors 12 10.0
                                   (fn []
                                     (seac/sim-load!
                                      (mapv #(assoc % :y y :omega 0.0)
                                            (seac/sim-particles))
                                      0.0))))
          trough (apply max (map brightness (at -0.35)))
          crest (apply min (map brightness (at 0.25)))]
      (is (< trough crest)
          "a trough is darker than a crest, so the shape of the sea reads"))))
