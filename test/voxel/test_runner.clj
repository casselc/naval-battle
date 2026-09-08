(ns voxel.test-runner
  (:require [clojure.test :as t])
  (:gen-class))

(defn -main
  [& _]
  (require '[voxel.mesh-test])
  (require '[voxel.camera-test])
  (require '[voxel.world-test])
  (require '[voxel.buoyancy-test])
  (require '[voxel.ocean-test])
  (require '[voxel.box3d-test])
  (require '[voxel.physics-test])
  (require '[voxel.ship-test])
  (require '[voxel.seac-test])
  (require '[voxel.input-test])
  (let [{:keys [fail error]} (t/run-tests 'voxel.mesh-test 'voxel.camera-test 'voxel.world-test 'voxel.buoyancy-test 'voxel.ocean-test 'voxel.box3d-test 'voxel.physics-test 'voxel.ship-test 'voxel.seac-test 'voxel.input-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
