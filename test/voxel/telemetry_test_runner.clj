(ns voxel.telemetry-test-runner
  (:require [clojure.test :as t])
  (:gen-class))

(defn -main [& _]
  (require '[voxel.telemetry-test])
  (let [{:keys [fail error]} (t/run-tests 'voxel.telemetry-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
