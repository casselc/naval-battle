(ns voxel.main
  "Window setup + the frame loop: input -> physics facts -> world -> render."
  (:require [voxel.raylib :as rl]
            [voxel.input :as input]
            [voxel.world :as w]
            [voxel.physics :as phys]
            [voxel.render :as render]))

(def WIDTH 960)
(def HEIGHT 540)
(def MAX-DEBRIS 240)
(def EXPLOSION-STRENGTH 0.35)

;; headless smoke: fire one scripted shot so a screenshot can capture a hit
(def ^:private autofire-frame
  (when-let [v (System/getenv "VOXEL_APP_AUTOFIRE")]
    (try (Integer/parseInt v) (catch Exception _ nil))))

;; headless smoke: override the 60fps cap (0 = uncapped) to measure the
;; true frame-work time
(def ^:private refire-every
  (when-let [v (System/getenv "VOXEL_APP_REFIRE")]
    (try (Integer/parseInt v) (catch Exception _ nil))))

(def ^:private smoke-fps
  (when-let [v (System/getenv "VOXEL_APP_FPS")]
    (try (Integer/parseInt v) (catch Exception _ nil))))

;; headless smoke: scripted helm "thrust,turn" so scripted runs can show
;; ships underway without a keyboard
(def ^:private smoke-helm
  (when-let [v (System/getenv "VOXEL_APP_HELM")]
    (try (mapv #(Double/parseDouble %) (clojure.string/split v #","))
         (catch Exception _ nil))))

;; live development: with VOXEL_APP_NREPL=port the game process hosts an
;; nREPL server, so namespaces reload while the loop runs. restart* and
;; shot* are REPL handles consumed inside the frame loop - the REPL never
;; calls raylib directly (GL is thread-confined). defonce so reloading
;; voxel.main does not wipe a pending request.
(defonce restart* (volatile! false))  ; (vreset! voxel.main/restart* true)
(defonce shot* (volatile! nil))       ; (vreset! voxel.main/shot* "live.png")

(defn- spawn-debris
  "Blasts and splashes kick up debris and spray cubes."
  [debris events]
  (let [new (mapcat (fn [ev]
                      (let [[x y z] (:point ev)
                            water (= :splash (:type ev))
                            cnt (max 3 (min 12 (or (:destroyed ev) 6)))]
                        (map (fn [_]
                               (let [a (/ (rl/get-random-value 0 628) 100.0)]
                                 {:x (double x) :y (double y) :z (double z)
                                  :vx (* 6.0 (Math/sin a))
                                  :vy (+ 5.0 (rl/get-random-value 0 60) 0.0)
                                  :vz (* 6.0 (Math/cos a))
                                  :size 0.3
                                  :color (if water
                                           (rl/rgba 200 228 240 255)
                                           (rl/rgba 148 151 165 255))
                                  :ttl (+ 0.7 (/ (rl/get-random-value 0 100) 100.0))}))
                             (range cnt))))
                    events)]
    (into [] (take-last MAX-DEBRIS (into debris new)))))

(defn- step-debris
  [debris dt]
  (into []
        (comp
         (filter #(> (:ttl %) 0))
         (map (fn [d]
                (let [vy (- (:vy d) (* 12.0 dt))]
                  (-> d
                      (assoc :x (+ (:x d) (* (:vx d) dt))
                             :y (+ (:y d) (* (:vy d) dt))
                             :z (+ (:z d) (* (:vz d) dt))
                             :vy vy
                             :ttl (- (:ttl d) dt)))))))
        debris))

(defn- end-screen
  "Which screen are we on: :title at first, :end when the battle is decided."
  [screen world]
  (cond
    (= :title screen) :title
    (= :over (:phase world)) :end
    :else :game))

(defn -main
  [& _]
  (rl/window! :width WIDTH :height HEIGHT
              :title "naval battle - voxel warships on a particle ocean")
  (rl/set-target-fps (if (nil? smoke-fps) 60 smoke-fps))
  (when-let [port (System/getenv "VOXEL_APP_NREPL")]
    (try
      (require '[jolt.nrepl])
      ((resolve 'jolt.nrepl/start) (Integer/parseInt port))
      (println "[voxel] nREPL listening on" port)
      (catch Exception e (println "[voxel] nREPL failed:" e))))
  (phys/init!)
  (let [deadline (rl/auto-quit-deadline)
        summary (volatile! nil)
        sim-work* (volatile! 0.0)
        draw-work* (volatile! 0.0)
        phys-work* (volatile! 0.0)]
    (loop [frame 0
           world (w/initial-state)
           ;; smoke mode (VOXEL_APP_AUTOFIRE) skips the title screen
           screen (if autofire-frame :game :title)
           debris []
           consumed 0
           ttotal 0.0]
      (if (rl/keep-running? deadline)
        (let [t0 (System/currentTimeMillis)
              ;; raylib reports real frame time; shader compiles, GC and
               ;; window drags spike it to 0.1s+, which tunnels shells
               ;; through hulls. Both Box3D and the shell integrator want
               ;; bounded dt.
               dt (min 0.033 (double (rl/get-frame-time)))
               in (input/snapshot WIDTH HEIGHT)
               ;; title/end: any click or R restarts the round; the REPL
                ;; can force one via restart*
                restart-now (or @restart*
                                (and (not= :game screen)
                                     (or (:pressed? in) (:restart? in)))
                                (and (= :game screen) (:restart? in)))
                _ (when restart-now (vreset! restart* false) (phys/init!))
                world (if restart-now (w/initial-state) world)
                screen (if restart-now :game screen)
                consumed (if restart-now 0 consumed)
                aim (input/sea-point (:mx in) (:my in) WIDTH HEIGHT
                                     render/CAMERA-POS render/CAMERA-TARGET
                                     render/FOVY render/CAMERA-ORTHO)
               ;; scripted smoke-test shot: one salvo at the enemy
               autofire? (and (= :game screen)
                              (or (= frame autofire-frame)
                                  (and (pos? (or refire-every 0))
                                       (zero? (rem frame refire-every)))))
               fire-target (if autofire?
                             (let [e (get-in world [:ships :enemy :pos])]
                               [(e 0) (+ (e 1) 1.0) (e 2)])
                             aim)
               fire-now (or autofire?
                            (and (= :game screen)
                                 (:released? in)
                                 (= :playing (:phase world))))
               world (if fire-now (w/fire world :player fire-target) world)
                ;; the helm: arrow keys drive the player's hull (the smoke
                ;; env overrides the keyboard so scripted runs can steer)
                helm (or smoke-helm (:helm in))
                player-body (get-in world [:ships :player :body])
                _ (when (and player-body
                             (= :game screen)
                             (= :playing (:phase world))
                             (not (get-in world [:ships :player :sunk])))
                    (phys/steer! player-body (helm 0) (helm 1)))
                ;; the enemy fleet closes to gun range on its own
                enemy-body (get-in world [:ships :enemy :body])
                _ (when (and enemy-body
                             (= :game screen)
                             (= :playing (:phase world))
                             (not (get-in world [:ships :enemy :sunk])))
                    (let [[t r] (w/ai-helm world :enemy)]
                      (phys/steer! enemy-body t r)))
               ;; physics: step Box3D, fold the facts into the battle
               tp0 (System/currentTimeMillis)
               facts (phys/step! dt)
               _ (vswap! phys-work* + (- (System/currentTimeMillis) tp0))
               world (w/step-state world dt facts)
               ;; ships still without a physics body: frame zero, or a
               ;; restart just rebuilt the fleet
               pending (into {}
                             (keep (fn [entry]
                                     (let [id (key entry)
                                           s (val entry)]
                                       (when (nil? (:body s))
                                         [id (phys/spawn-body! (:pos s) (:quat s) 1
                                                               (:anchor s)
                                                               (keys (:cells s)))]))))
                             (:ships world))
               world (if (seq pending)
                       (w/attach-bodies world pending)
                       world)
               ;; fresh blasts: shove the hulls, and open their recorded
               ;; meshes so they start taking on water immediately
               _ (doseq [ev (drop consumed (:events world))
                         :when (= :blast (:type ev))
                         :let [body-id (get-in world [:ships (:ship ev) :body])]]
                   (phys/explode! (:point ev) (+ w/BLAST-RADIUS 0.5)
                                  EXPLOSION-STRENGTH)
                   (when body-id
                     (phys/damage-cells! body-id (:cells ev))))
               _ (phys/destroy-unreferenced! (w/live-body-ids world))
               fresh (filter #(contains? #{:blast :splash} (:type %))
                             (drop consumed (:events world)))
               debris' (step-debris (spawn-debris debris fresh) dt)
               consumed' (count (:events world))
               tsim (- (System/currentTimeMillis) t0)
               tdraw0 (System/currentTimeMillis)]
          (render/draw-frame! {:world world
                               :ui {:aim aim :mx (:mx in) :my (:my in)}
                               :debris debris'
                               :width WIDTH :height HEIGHT
                               :screen (end-screen screen world)})
          (rl/maybe-screenshot! frame 150)
           (when-let [s @shot*]
             (vreset! shot* nil)
             (rl/screenshot! s))
          (vswap! sim-work* + tsim)
          (vswap! draw-work* + (- (System/currentTimeMillis) tdraw0))
          (vreset! summary {:frame frame
                            :phase (:phase world)
                            :winner (:winner world)
                            :events (mapv :type (:events world))
                            :debris (count debris')
            :avg-frame-ms (when (pos? frame)
                            (double (* 1000.0 (/ ttotal frame))))
            :avg-sim-ms (when (pos? frame) (/ @sim-work* frame 1.0))
            :avg-draw-ms (when (pos? frame) (/ @draw-work* frame 1.0))
            :avg-phys-ms (when (pos? frame) (/ @phys-work* frame 1.0))})
          (recur (inc frame) world screen debris' consumed' (+ ttotal dt)))
        (when autofire-frame
          (println "[voxel] smoke summary:" (pr-str @summary))))))
  (rl/close-window))
