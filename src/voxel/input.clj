(ns voxel.input
  "Raylib input reads ONLY. Reports raw intents: mouse position, aim
  yaw/pitch, button edges, restart key. Owns no game state."
  (:require [voxel.raylib :as rl]))

(def YAW-MAX 0.35)      ; radians either side of straight downrange
(def PITCH-MIN 0.04)    ; near-flat shots
(def PITCH-MAX 0.85)    ; high lobs
(def CHARGE-TIME 1.1)   ; seconds of hold from zero to full power

(defn aim-from-mouse
  "Mouse position -> [yaw pitch]. Screen right aims right (+x), screen top
  aims higher."
  [mx my width height]
  [(double (* (- (/ (double mx) width) 0.5) 2.0 YAW-MAX))
   (-> (- 1.0 (/ (double my) height))
       (* PITCH-MAX)
       (max PITCH-MIN)
       (min PITCH-MAX))])

(defn power-from-charge
  "Charge seconds in [0, CHARGE-TIME] -> power in [0,1]."
  [charge-t]
  (max 0.0 (min 1.0 (/ (double charge-t) CHARGE-TIME))))

(defn snapshot
  "One frame of input as pure data."
  [width height]
  {:mx (rl/get-mouse-x)
    :my (rl/get-mouse-y)
    :aim (aim-from-mouse (rl/get-mouse-x) (rl/get-mouse-y) width height)
    :pressed? (rl/mouse-pressed? rl/MOUSE-LEFT)
    :down? (rl/mouse-down? rl/MOUSE-LEFT)
    :released? (rl/mouse-released? rl/MOUSE-LEFT)
    :restart? (rl/key-pressed? rl/KEY-R)
    ;; helm: [thrust turn]. Up/Down drive ahead/astern along the bow,
    ;; Left/Right yaw to port/starboard.
    :helm [(cond (rl/key-down? rl/KEY-UP) 1.0
                 (rl/key-down? rl/KEY-DOWN) -1.0
                 :else 0.0)
           (cond (rl/key-down? rl/KEY-RIGHT) 1.0
                 (rl/key-down? rl/KEY-LEFT) -1.0
                 :else 0.0)]})

(defn- cross3
  [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn- norm3
  [v]
  (let [l (Math/sqrt (reduce + (map #(* % %) v)))]
    (mapv #(/ % l) v)))

(defn sea-point
  "Mouse position -> the point where the camera ray through it crosses the
  flat sea (y = 0): where the player is aiming. Falls back to a far
  downrange point when the ray never dips below the horizon. The 8-arg
  arity serves the orthographic RTS camera (fovy = full vertical
  world-units in view); the 7-arg arity stays perspective."
  ([mx my width height cam-pos cam-target fovy]
   (sea-point mx my width height cam-pos cam-target fovy false))
  ([mx my width height cam-pos cam-target fovy ortho?]
   (let [f (norm3 (mapv - cam-target cam-pos))
         r (norm3 (cross3 f [0.0 1.0 0.0]))
         u (cross3 r f)
         a (/ (double width) (double height))
         ndc-x (- (* 2.0 (/ (double mx) width)) 1.0)
         ndc-y (- 1.0 (* 2.0 (/ (double my) height)))
         dir (if ortho?
               f
               (norm3 (mapv + f
                            (mapv #(* ndc-x (Math/tan (Math/toRadians (/ fovy 2.0))) a %) r)
                            (mapv #(* ndc-y (Math/tan (Math/toRadians (/ fovy 2.0))) %) u))))
         origin (if ortho?
                  (let [half-h (* 0.5 fovy)
                        half-w (* half-h a)]
                    (mapv + cam-pos
                          (mapv #(* ndc-x half-w %) r)
                          (mapv #(* ndc-y half-h %) u)))
                  cam-pos)
         dy (dir 1)
         s (if (< dy -1e-4) (- (/ (- (origin 1)) dy)) 400.0)
         x (+ (origin 0) (* s (dir 0)))
         z (+ (origin 2) (* s (dir 2)))
         clamp (fn [v] (max -70.0 (min 70.0 v)))]
     [(clamp x) 0.0 (clamp z)])))
