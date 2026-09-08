(ns voxel.telemetry.hud-overlay
  "Compact, render-thread-safe view of the background HUD snapshot.

  This namespace never owns or queries telemetry. `draw-current!` performs one
  atom-backed snapshot read and bounded raylib drawing before EndDrawing."
  (:require [clojure.string :as str]
            [voxel.raylib :as rl]
            [voxel.telemetry.hud :as hud]))

(def ^:private max-rows-per-signal 3)
(def ^:private max-label-codepoints 34)

(defn- bounded-label [value]
  (let [text (-> (if (nil? value) "_OTHER" (str value))
                 (str/replace #"[\r\n\t]+" " ")
                 str/trim)]
    (cond
      (empty? text) "_OTHER"
      (> (count text) max-label-codepoints)
      (str (subs text 0 (- max-label-codepoints 3)) "...")
      :else text)))

(defn- bounded-count [value]
  (cond
    (not (and (integer? value) (not (neg? value)))) "0"
    (> value 999999) "999999+"
    :else (str value)))

(defn- signal-lines [title rows]
  (into [title]
        (map (fn [{:keys [value count]}]
               (str "  " (bounded-label value) "  " (bounded-count count))))
        (take max-rows-per-signal (or rows []))))

(defn model-lines
  "Return the complete bounded line model drawn by the HUD."
  [model]
  (let [status (case (:status model)
                 :ready "LIVE"
                 :stale "STALE"
                 :starting "STARTING"
                 "UNAVAILABLE")]
    (into [(str "OSCOPE TELEMETRY  " status)]
          (concat (signal-lines "SPANS" (:spans model))
                  (signal-lines "METRICS" (:metrics model))))))

(defn draw-model!
  "Draw an already-cached model with a supplied small drawing API.

  The injectable API makes the render seam testable without a display or
  native raylib. It must contain `:rectangle!`, `:rectangle-lines!`, and
  `:text!` functions with raylib's scalar signatures."
  [model {:keys [rectangle! rectangle-lines! text!]}]
  (let [lines (model-lines model)
        x 14
        y 14
        line-height 18
        width 370
        height (+ 16 (* line-height (count lines)))]
    (rectangle! x y width height (rl/rgba 7 14 24 218))
    (rectangle-lines! x y width height (rl/rgba 69 168 255 255))
    (doseq [[index line] (map-indexed vector lines)]
      (text! line (+ x 10) (+ y 8 (* index line-height)) 14
             (if (zero? index) rl/SKYBLUE rl/RAYWHITE))))
  nil)

(defn draw-current!
  "Read the active sampler's immutable cache once and draw its bounded model."
  []
  (when-let [model (hud/snapshot)]
    (draw-model! model {:rectangle! rl/draw-rectangle
                        :rectangle-lines! rl/draw-rectangle-lines
                        :text! rl/draw-text}))
  nil)
