(ns voxel.render
  "Raylib draw calls ONLY. Draws a naval battle state (voxel.world) - two
  warships on the particle ocean, shells in flight, the aim arc - plus the
  HUD overlay. No game logic here."
  (:require [voxel.camera :as cam]
            [voxel.raylib :as rl]
            [voxel.seac :as seac]
            [voxel.world :as w]))

;; the vantage itself lives in voxel.camera, which is pure - the ocean sizes
;; itself from the same numbers so the water always runs past the frame
(def CAMERA-POS cam/POS)
(def CAMERA-TARGET cam/TARGET)
(def FOVY cam/FOVY)
(def CAMERA-ORTHO cam/ORTHO)
;; the title screen and the tools get the fixed vantage; the frame loop
;; hands draw-frame! the one that is following the battle
(def DEFAULT-CAMERA {:pos cam/POS :target cam/TARGET :fovy cam/FOVY})

(def MAT-COLORS
  {:hull (rl/rgba 72 94 112 255)       ; steel grey-blue
   :deck (rl/rgba 133 108 78 255)      ; weathered teak
   :super (rl/rgba 152 158 168 255)    ; superstructure
   :gun (rl/rgba 58 60 68 255)})       ; turret dark

(def DEEP (rl/rgba 18 62 104 255))    ; the colour of a trough
(def SWELL (rl/rgba 46 124 170 255))  ; the colour of a crest
(def FOAM (rl/rgba 208 232 240 255))

;; a late-afternoon sun from the west-northwest; HALF-VIEW is the halfway
;; vector between it and the camera, for specular sparkle on wave crests
(def SUN-L (let [[x y z] [-0.45 0.78 0.35]
                 l (Math/sqrt (+ (* x x) (* y y) (* z z)))]
             [(/ x l) (/ y l) (/ z l)]))
(def HALF-VIEW
  (let [v (mapv - CAMERA-TARGET CAMERA-POS)
        x (+ (SUN-L 0) (v 0)) y (+ (SUN-L 1) (v 1)) z (+ (SUN-L 2) (v 2))
        l (Math/sqrt (+ (* x x) (+ (* y y) (* z z))))]
    [(/ x l) (/ y l) (/ z l)]))

(defn- channel
  [c s]
  (bit-and (bit-shift-right c s) 0xff))

(defn- cell-color
  "Material colour with a deterministic per-cell tint so hulls do not read
  as flat slabs; wrecks shade down toward the deep."
  [cell mat sunk?]
  (let [[i j k] cell
        v (mod (+ i j k) 3)
        f (case v 0 1.0 1 0.93 2 0.86)]
    (rl/shade (get MAT-COLORS mat) (if sunk? (* 0.55 f) f))))

;; defonce: surviving ns reloads keeps the C meshes valid instead of
;; leaking them (ids are {:id mesh-id :sig [face-count sunk] :ext [ex ez]},
;; rebuilt only when damage or sinking changes the hull)
(defonce ship-meshes (volatile! {}))

(defn ship-extent
  "Hull footprint semi-axes [ex ez] about the anchor, in WORLD units, from
  its face cells. Cell indices are grid coordinates: the anchor has to come
  out of them and the voxel size has to go in, or the shadow is drawn at
  whatever the grid resolution happens to be."
  ([faces anchor] (ship-extent faces anchor 1.0))
  ([faces anchor voxel]
   (let [ax (double (anchor 0))
         az (double (anchor 2))
         s (double voxel)
         reach (fn [sel a]
                 (reduce (fn [m face]
                           (let [c (double (sel (first face)))]
                             (max m (Math/abs (- (+ c 0.5) a)))))
                         0.0 faces))]
     [(* s (+ 0.5 (reach #(% 0) ax)))
      (* s (+ 0.5 (reach #(% 2) az)))])))

(defn- ensure-ship-mesh!
  "The C mesh for one hull, rebuilt only when its signature (exposed-face
  count, sunk) changes: jolt owns the per-face colours at build time -
  material, per-cell tint, wreck shading - and C owns the per-frame
  rotate/cull/shade/submit."
  [id {:keys [cells faces sunk] :as ship}]
  (let [sig [(count faces) sunk]
        st (get @ship-meshes id)]
    (if (and st (= sig (:sig st)))
      (:id st)
      (do (when st (seac/ship-free! (:id st)))
          (let [fseq (seq faces)
                colors (mapv (fn [[cell _dir]]
                               (cell-color cell (get cells cell) sunk))
                             fseq)
                mid (seac/ship-init! fseq colors (:voxel ship 1.0))]
            (vswap! ship-meshes assoc id
                    {:id mid :sig sig
                     :ext (ship-extent fseq (:anchor ship) (:voxel ship 1.0))})
            mid)))))

(defn- draw-ship!
  "A warship as ONE C-submitted mesh: per-face colours were computed at
  build; per frame C rotates the faces by the hull quaternion, culls those
  hidden from the camera, sun-shades by the live world normal and draws."
  [id ship cam-pos]
  (let [mid (ensure-ship-mesh! id ship)]
    (when (>= mid 0)
      (seac/ship-draw! mid (:pos ship) (:quat ship) (:anchor ship)
                       SUN-L cam-pos))))

;; defonce so an ns reload keeps the C mesh rather than leaking it
(defonce sea-mesh-ready? (volatile! false))

(defn- draw-sea!
  "The whole ocean as ONE mesh built and drawn in C, one vertex per particle:
  the surface a player sees is the particle heights and nothing else. No
  vertex data crosses the FFI boundary - C fills the mesh from the same
  arrays the sim steps."
  []
  ;; a restart re-lays the sim, so rebuild whenever the sheet no longer
  ;; matches the particle set rather than quietly drawing a stale one
  (when (or (not @sea-mesh-ready?)
            (not= (seac/mesh-vertex-count) (seac/sim-count)))
    (seac/mesh-init!)
    (vreset! sea-mesh-ready? true))
  (seac/mesh-update! SUN-L HALF-VIEW DEEP SWELL FOAM)
  (seac/mesh-draw!))

(def ^:private SHADOW-ALPHA 96)

(defn- draw-shadows!
  "Hull shadows as soft dark ellipses on the water, offset from the hull
  along the sun rays. Reads as a cast shadow from the battle camera
  without per-face projection, and the waves show through them."
  [ships]
  (rl/rl-set-blend rl/GL-SRC-ALPHA rl/GL-ONE-MINUS-SRC-ALPHA rl/GL-FUNC-ADD)
  (rl/rl-begin rl/RL-TRIANGLES)
  (doseq [[id {:keys [pos sunk]}] ships
          :when (and (not sunk) (> (pos 1) -4.5))
          :let [[ex ez] (or (:ext (get @ship-meshes id)) [5.0 15.0])
                h (max 0.5 (- (+ (pos 1) 2.5)))
                cx (+ (pos 0) (* (- (SUN-L 0)) (/ h (SUN-L 1))))
                cz (+ (pos 2) (* (- (SUN-L 2)) (/ h (SUN-L 1))))
                y 0.28]]
    (rl/rl-color! (rl/rgba 6 18 36 SHADOW-ALPHA))
    (dotimes [s 12]
      (let [a0 (/ (* 2.0 Math/PI s) 12.0)
            a1 (/ (* 2.0 Math/PI (inc s)) 12.0)]
        (rl/rl-vertex-3f (double cx) (double y) (double cz))
        (rl/rl-vertex-3f (double (+ cx (* ex (Math/cos a0))))
                         (double y)
                         (double (+ cz (* ez (Math/sin a0)))))
        (rl/rl-vertex-3f (double (+ cx (* ex (Math/cos a1))))
                         (double y)
                         (double (+ cz (* ez (Math/sin a1))))))))
  (rl/rl-end))

(defn- draw-shells!
  [shells]
  (doseq [{:keys [pos]} shells]
    (rl/sphere! :pos pos :radius 0.3 :rings 8 :slices 12 :color rl/DARKGRAY)))

(defn- draw-arc!
  "Dotted ballistic preview to the aim point, when the guns are ready and a
  solution exists."
  [world target]
  (when-let [pts (w/preview-arc world :player target)]
    (rl/rl-begin rl/RL-LINES)
    (rl/rl-color! rl/WHITE)
    (doseq [[[x1 y1 z1] [x2 y2 z2]] (partition 2 2 pts)]
      (rl/rl-vertex-3f (double x1) (double y1) (double z1))
      (rl/rl-vertex-3f (double x2) (double y2) (double z2)))
    (rl/rl-end)))

(defn- draw-aim!
  "A flat gold tile where the mouse ray meets the sea."
  [[x _ z]]
  (rl/cube! :pos [x 0.06 z] :size [1.6 0.08 1.6] :color rl/GOLD))

(defn- draw-debris!
  [debris]
  (doseq [{:keys [x y z size color]} debris]
    (rl/cube! :pos [x y z] :size size :color color)))

(defn- draw-hud!
  [world width height]
  (let [player (get-in world [:ships :player])
        enemy (get-in world [:ships :enemy])
        cd (or (:cooldown player) 0.0)
        ready? (<= cd 0.0)
        bar-w 220
        frac (max 0.0 (min 1.0 (- 1.0 (/ cd w/FIRE-COOLDOWN))))
        right (fn [s size]
                (- width 14 (rl/text-width s :size size)))]
    (rl/text! (if ready? "GUNS READY" "RELOADING")
              :x 14 :y 14 :size 20 :color (if ready? rl/GREEN rl/YELLOW))
    (rl/rect! :x 14 :y 40 :width bar-w :height 10 :color rl/DARKGRAY)
    (rl/rect! :x 14 :y 40 :width (int (* bar-w frac)) :height 10 :color rl/GREEN)
    (rl/text! (str "YOUR HULL  " (count (:cells player)) " cells")
              :x 14 :y 62 :size 18 :color rl/WHITE)
    (let [es (if (:sunk enemy)
               "ENEMY  SUNK"
               (str "ENEMY HULL  " (count (:cells enemy)) " cells"))]
      (rl/text! es :x (right es 18) :y 14 :size 18
                :color (if (:sunk enemy) rl/RED rl/WHITE)))
    (let [ss (str "SHELLS IN FLIGHT  " (count (:shells world)))]
      (rl/text! ss :x (right ss 16) :y 40 :size 16 :color rl/LIGHTGRAY))
    (rl/text! "aim with the mouse - LMB to fire - R restart"
              :x 14 :y (- height 24) :size 16 :color rl/LIGHTGRAY)))

(defn- draw-crosshair!
  [mx my]
  (rl/line! :x1 (- mx 12) :y1 my :x2 (- mx 4) :y2 my :color rl/WHITE)
  (rl/line! :x1 (+ mx 4) :y1 my :x2 (+ mx 12) :y2 my :color rl/WHITE)
  (rl/line! :x1 mx :y1 (- my 12) :x2 mx :y2 (- my 4) :color rl/WHITE)
  (rl/line! :x1 mx :y1 (+ my 4) :x2 mx :y2 (+ my 12) :color rl/WHITE))

(defn- draw-overlay!
  [title subtitle width height]
  (rl/rect! :x 0 :y 0 :width width :height height :color (rl/rgba 10 12 20 190))
  ;; measured text widths are often odd, and / on odd ints yields a ratio,
  ;; which DrawText's :int FFI arg rejects - quot keeps every centre an int
  (let [centre-x (fn [text-w] (- (quot width 2) (quot text-w 2)))
        t-w (rl/text-width title :size 44)
        s-w (rl/text-width subtitle :size 20)]
    (rl/text! title :x (centre-x t-w) :y (- (quot height 2) 70)
              :size 44 :color rl/GOLD)
    (rl/text! subtitle :x (centre-x s-w) :y (- (quot height 2) 6)
              :size 20 :color rl/WHITE)))

(defn draw-frame!
  "Everything for one frame. `camera` is the live vantage from voxel.camera;
  without one it falls back to the fixed default."
  [{:keys [world ui debris width height screen camera]}]
  (let [{:keys [pos target fovy]} (or camera DEFAULT-CAMERA)]
  (rl/begin-drawing)
  (rl/clear-background (rl/rgba 120 160 200 255))
  (rl/with-camera-3d {:pos-x (pos 0) :pos-y (pos 1) :pos-z (pos 2)
                      :target-x (target 0) :target-y (target 1)
                      :target-z (target 2)
                      :fovy fovy :projection CAMERA-ORTHO}
    (fn []
      (draw-sea!)
      (draw-shadows! (:ships world))
      (doseq [[id ship] (:ships world)]
        (draw-ship! id ship pos))
      (draw-shells! (:shells world))
      (when (= :game screen)
        (draw-aim! (:aim ui))
        (draw-arc! world (:aim ui)))
      (draw-debris! debris)))
  (when (= :game screen)
    (draw-hud! world width height)
    (draw-crosshair! (:mx ui) (:my ui)))
  (when (= :title screen)
    (draw-overlay! "NAVAL BATTLE"
                   "click to start - arrows steer, click to fire - sink the enemy dreadnought" width height))
  (when (= :end screen)
    (if (= :player (:winner world))
      (draw-overlay! "VICTORY"
                     "the enemy went down - R or click to fight again" width height)
      (draw-overlay! "DEFEAT"
                     "your fleet is lost - R or click to try again" width height)))
  (rl/fps! :x 8 :y 8)
  (rl/end-drawing)))
