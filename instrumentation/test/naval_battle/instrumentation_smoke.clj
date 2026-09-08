(ns naval-battle.instrumentation-smoke
  "Deterministic real-source workload shared by plain and woven executables."
  (:require [otel.sdk :as sdk]
            [voxel.input :as input]
            [voxel.main]
            [voxel.ocean :as ocean]
            ;; Requiring these namespaces makes their real entry seams part of
            ;; the build/report. This headless driver deliberately does not call
            ;; native physics or rendering functions.
            [voxel.physics]
            [voxel.render]
            [voxel.world :as world]))

(defn- tiny-state []
  {:phase :playing
   :ships
   {:player {:id :player :ai false :cells {[0 0 0] true}
             :anchor [0.0 0.0 0.0] :guns [[0 0 0]] :voxel 1.0
             :skin #{} :faces #{} :pos [0.0 0.0 0.0]
             :quat [0.0 0.0 0.0 1.0] :body nil
             :vel [0.0 0.0 0.0] :speed 0.0 :cooldown 0.0 :sunk false}
    :enemy {:id :enemy :ai false :cells {[0 0 0] true}
            :anchor [0.0 0.0 0.0] :guns [[0 0 0]] :voxel 1.0
            :skin #{} :faces #{} :pos [0.0 0.0 30.0]
            :quat [0.0 0.0 0.0 1.0] :body nil
            :vel [0.0 0.0 0.0] :speed 0.0 :cooldown 0.0 :sunk false}}
   :shells []
   :ocean (assoc (ocean/make-ocean [{:x 0.0 :z 0.0 :omega 0.0}])
                 :ambient false)
   :time 0.0
   :events []})

(defn -main [& _]
  (let [handle (sdk/init! {:service-name "naval-battle-case-study"
                           :processor :simple
                           :metrics? true
                           :runtime-metrics? false
                           :logs? true
                           :bridge-logging? false})]
    (try
      (let [aim (input/aim-from-mouse 320 240 1280 720)
            _match (world/initial-state)
            fired (world/fire (tiny-state) :player [0.0 0.0 25.0]
                              world/SHELL-SPEED)
            stepped (world/step-state fired 0.01 {:bodies []})]
        (sdk/force-flush! handle)
        ;; This is the application oracle compared byte-for-byte between the
        ;; plain and woven executables. No telemetry identity enters it.
        (prn {:aim aim
              :shells-before (count (:shells (tiny-state)))
              :shells-after-fire (count (:shells fired))
              :time-after-step (:time stepped)
              :phase-after-step (:phase stepped)}))
      (finally
        (sdk/shutdown! handle)))))
