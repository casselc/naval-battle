(ns voxel.telemetry.main
  "Opt-in game launcher: unchanged naval battle inside a Durable OTel shell."
  (:require [voxel.main :as game]
            [voxel.telemetry.config :as config]
            ;; Retain the optional advice target in self-contained builds. The
            ;; fork-local provider resolves it only at the frame-present seam.
            [voxel.telemetry.hud-overlay]
            [voxel.telemetry.runtime :as telemetry]))

(defn -main [& args]
  (let [runtime (telemetry/start! (config/env-options))
        stop! #(telemetry/stop-until-closed! runtime)
        shutdown-hook (Thread. stop!)]
    (.addShutdownHook (Runtime/getRuntime) shutdown-hook)
    (println "[voxel] embedded telemetry viewer:" (get-in runtime [:viewer :url]))
    (try
      (apply game/-main args)
      (finally
        (stop!)
        (.removeShutdownHook (Runtime/getRuntime) shutdown-hook)))))
