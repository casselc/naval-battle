(ns voxel.render
  "Raylib draw calls ONLY. Draws a naval battle state (voxel.world) - two
  warships on the particle ocean, shells in flight, the aim arc - plus the
  HUD overlay. No game logic here."
  (:require [voxel.raylib :as rl]
            [voxel.world :as w]))

;; player vantage: behind and above the player's stern, looking downrange
;; at the enemy fleet
(def CAMERA-POS [0.0 16.0 -46.0])
(def CAMERA-TARGET [0.0 1.0 6.0])
(def FOVY 55.0)

(def MAT-COLORS
  {:hull (rl/rgba 72 94 112 255)       ; steel grey-blue
   :deck (rl/rgba 133 108 78 255)      ; weathered teak
   :super (rl/rgba 152 158 168 255)    ; superstructure
   :gun (rl/rgba 58 60 68 255)})       ; turret dark

(def DEEP (rl/rgba 16 58 96 255))
(def SWELL (rl/rgba 30 96 138 255))
(def FOAM (rl/rgba 208 232 240 255))

;; a late-afternoon sun from the west-northwest; HALF-VIEW is the halfway
;; vector between it and the camera, for specular sparkle on wave crests
(def SUN-L (let [[x y z] [-0.45 0.78 0.35]
                 l (Math/sqrt (+ (* x x) (* y y) (* z z)))]
             [(/ x l) (/ y l) (/ z l)]))
(def HALF-VIEW
  (let [v [(- 0.0 (CAMERA-POS 0)) (- 16.0 (CAMERA-POS 1)) (- 6.0 (CAMERA-POS 2))]
        x (+ (SUN-L 0) (v 0)) y (+ (SUN-L 1) (v 1)) z (+ (SUN-L 2) (v 2))
        l (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    [(/ x l) (/ y l) (/ z l)]))

(defn- channel
  [c s]
  (bit-and (bit-shift-right c s) 0xff))

(defn- mix
  "Linear blend of two packed colors."
  [c1 c2 t]
  (rl/rgba (int (+ (channel c1 0) (* t (- (channel c2 0) (channel c1 0)))))
           (int (+ (channel c1 8) (* t (- (channel c2 8) (channel c1 8)))))
           (int (+ (channel c1 16) (* t (- (channel c2 16) (channel c1 16)))))
           255))

(defn- scale-color
  "Multiply a packed color's channels by f (may exceed 1, clamped to 255)."
  [c f]
  (rl/rgba (min 255 (int (* f (channel c 0))))
           (min 255 (int (* f (channel c 8))))
           (min 255 (int (* f (channel c 16))))
           255))

(defn- lift-color
  "Blend a packed color toward white by t (specular highlight)."
  [c t]
  (mix c (rl/rgba 255 255 255 255) t))

(defn- cell-color
  "Material colour with a deterministic per-cell tint so hulls do not read
  as flat slabs; wrecks shade down toward the deep."
  [cell mat sunk?]
  (let [[i j k] cell
        v (mod (+ i j k) 3)
        f (case v 0 1.0 1 0.93 2 0.86)]
    (rl/shade (get MAT-COLORS mat) (if sunk? (* 0.55 f) f))))

(def ^:private DIR-TRIS
  "Per face direction: the six corner-offsets (two triangles) of the exposed
  unit-cell quad, mesh.clj's outward winding. Ships draw one quad per cached
  exposed face instead of a full cube per surface cell."
  {[1 0 0]  [[1 0 0] [1 1 0] [1 1 1] [1 0 0] [1 1 1] [1 0 1]]
   [-1 0 0] [[0 0 0] [0 0 1] [0 1 1] [0 0 0] [0 1 1] [0 1 0]]
   [0 1 0]  [[0 1 0] [0 1 1] [1 1 1] [0 1 0] [1 1 1] [1 1 0]]
   [0 -1 0] [[0 0 0] [1 0 0] [1 0 1] [0 0 0] [1 0 1] [0 0 1]]
   [0 0 1]  [[0 0 1] [1 0 1] [1 1 1] [0 0 1] [1 1 1] [0 1 1]]
   [0 0 -1] [[0 0 0] [0 1 0] [1 1 0] [0 0 0] [1 1 0] [1 0 0]]})

(def ^:private DIR-SHADE
  "Face-direction depth shading, the same cues cube! used."
  {[0 1 0] 1.0 [0 0 1] 1.0 [1 0 0] 0.85 [-1 0 0] 0.7 [0 0 -1] 0.5 [0 -1 0] 0.4})

(defn- draw-ship!
  "A warship at its physics transform: translate to the hull position,
  rotate by its quaternion, and draw the cached exposed faces relative to
  the anchor the body was spawned around - one quad per face."
  [{:keys [pos quat anchor cells faces sunk]}]
  (rl/rl-push-matrix)
  (rl/rl-translate-f (double (pos 0)) (double (pos 1)) (double (pos 2)))
  (rl/rl-rotate-quaternion! quat)
  (rl/rl-begin rl/RL-TRIANGLES)
  (let [[ax ay az] anchor]
    (doseq [[cell dir] faces
            :let [[i j k] cell
                  base (rl/shade (cell-color cell (get cells cell) sunk)
                                 (get DIR-SHADE dir))]]
      (rl/rl-color! base)
      ;; ffi :float slots want doubles - integral anchors make exact-long
      ;; coordinates that the foreign conversion rejects
      (doseq [o (get DIR-TRIS dir)]
        (rl/rl-vertex-3f (double (- (+ i (o 0)) ax))
                         (double (- (+ j (o 1)) ay))
                         (double (- (+ k (o 2)) az))))))
  (rl/rl-end)
  (rl/rl-pop-matrix))

(defn- ambient-swell
  "A gentle visual-only swell so the sea reads as water even before the
  battle kicks it around: three crossing travelling sine waves. The physics
  particles' own spray height rides on top of it."
  [x z t]
  (+ (* 0.09 (Math/sin (+ (* 0.8 x) (* 1.3 t))))
     (* 0.07 (Math/sin (- (* 0.5 z) (* 1.1 t))))
     (* 0.05 (Math/sin (+ (* 0.4 (+ x z)) (* 0.7 t))))))

(defn- sea-grid
  "Particles keyed by their grid cell, for height-field gradients."
  [particles]
  (into {} (map (fn [p]
                  [[(Math/round (/ (:x p) 2.0)) (Math/round (/ (:z p) 2.0))] p])
                particles)))

(defn- tile-normal
  "Surface normal from central differences over the tile height field."
  [grid i j]
  (let [h (fn [ii jj] (or (:y (get grid [ii jj])) 0.0))
        gx (/ (- (h (inc i) j) (h (dec i) j)) 4.0)
        gz (/ (- (h i (inc j)) (h i (dec j))) 4.0)
        l (Math/sqrt (+ 1.0 (* gx gx) (* gz gz)))]
    [(/ (- gx) l) (/ 1.0 l) (/ (- gz) l)]))

(defn- hull-shadow
  "1.0 in open water, falling to 0.0 under a floating hull's footprint (a
  soft ellipse the length of the ship), so vessels sit ON the sea instead of
  floating above it."
  [ships x z]
  (reduce (fn [b s]
            (let [sy ((:pos s) 1)]
              (if (or (:sunk s) (< sy -4.5))
                b
                (let [dx (/ (- x ((:pos s) 0)) 4.5)
                      dz (/ (- z ((:pos s) 2)) 9.0)
                      d (Math/sqrt (+ (* dx dx) (* dz dz)))]
                  (min b (max 0.0 (min 1.0 (/ (- d 0.75) 0.5))))))))
          1.0 (vals ships)))

(defn- draw-sea!
  "The particle ocean: one deep plane out to the horizon, then every ocean
  particle as a surface tile. Tiles are LIT: the wave normal from the local
  height field diffuses against the sun, crests aligned with the view
  sparkle, hulls shadow the water under them, and spray/vorticity whiten
  toward foam."
  [ocean ships t]
  (rl/rl-begin rl/RL-TRIANGLES)
  (rl/rl-color! DEEP)
  (rl/rl-vertex-3f -70.0 0.0 -70.0)
  (rl/rl-vertex-3f 70.0 0.0 70.0)
  (rl/rl-vertex-3f 70.0 0.0 -70.0)
  (rl/rl-vertex-3f -70.0 0.0 -70.0)
  (rl/rl-vertex-3f -70.0 0.0 70.0)
  (rl/rl-vertex-3f 70.0 0.0 70.0)
  (rl/rl-end)
  (let [grid (sea-grid (mapv (fn [p]
                               (assoc p :y (+ (or (:y p) 0.0)
                                              (ambient-swell (:x p) (:z p) t))))
                             (:particles ocean)))]
    (rl/rl-begin rl/RL-TRIANGLES)
    (doseq [[i j :as cell] (keys grid)]
      (let [p (get grid cell)
            y (:y p)
            n (tile-normal grid i j)
            diffuse (max 0.0 (+ (* (n 0) (SUN-L 0)) (* (n 1) (SUN-L 1)) (* (n 2) (SUN-L 2))))
            spec (Math/pow (max 0.0 (+ (* (n 0) (HALF-VIEW 0))
                                       (* (n 1) (HALF-VIEW 1))
                                       (* (n 2) (HALF-VIEW 2))))
                           24.0)
            foam (min 1.0 (+ (* 0.8 (or (:y p) 0.0))
                             (* 0.3 (Math/abs (or (:omega p) 0.0)))))
            shade (min 1.0 (+ (* 0.45 diffuse)
                              (* 0.55 (hull-shadow ships (:x p) (:z p)))))
            base (mix SWELL FOAM foam)
            ;; a single top quad per tile: the camera flies above the sea
            ty (+ 0.08 (* 0.6 y))
            x0 (- (:x p) 0.99) x1 (+ (:x p) 0.99)
            z0 (- (:z p) 0.99) z1 (+ (:z p) 0.99)]
        (rl/rl-color! (lift-color (scale-color base (* 1.9 shade)) (* 0.7 spec)))
        (rl/rl-vertex-3f x0 ty z1) (rl/rl-vertex-3f x1 ty z1)
        (rl/rl-vertex-3f x1 ty z0) (rl/rl-vertex-3f x0 ty z1)
        (rl/rl-vertex-3f x1 ty z0) (rl/rl-vertex-3f x0 ty z0)))
    (rl/rl-end)))

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
  "Everything for one frame."
  [{:keys [world ui debris width height screen]}]
  (rl/begin-drawing)
  (rl/clear-background (rl/rgba 120 160 200 255))
  (rl/with-camera-3d {:pos-x (CAMERA-POS 0) :pos-y (CAMERA-POS 1) :pos-z (CAMERA-POS 2)
                      :target-x (CAMERA-TARGET 0) :target-y (CAMERA-TARGET 1)
                      :target-z (CAMERA-TARGET 2)
                      :fovy FOVY :projection 0}
    (fn []
      (draw-sea! (:ocean world) (:ships world) (or (:time world) 0.0))
      (doseq [ship (vals (:ships world))]
        (draw-ship! ship))
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
                   "click to start - sink the enemy dreadnought" width height))
  (when (= :end screen)
    (if (= :player (:winner world))
      (draw-overlay! "VICTORY"
                     "the enemy went down - R or click to fight again" width height)
      (draw-overlay! "DEFEAT"
                     "your fleet is lost - R or click to try again" width height)))
  (rl/fps! :x 8 :y 8)
  (rl/end-drawing))
