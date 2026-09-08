(ns naval-battle.instrumentation-report-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]))

(def expected
  {[:game/frame-input 'voxel.input/snapshot 2]
   'naval-battle.instrumentation/around-input
   [:game/match-start 'voxel.world/initial-state 0]
   'naval-battle.instrumentation/around-session
   [:game/physics-step 'voxel.physics/step! 2]
   'naval-battle.instrumentation/around-physics
   [:game/render-frame 'voxel.render/draw-frame! 1]
   'naval-battle.instrumentation/around-render
   [:game/frame-present 'voxel.raylib/end-drawing 0]
   'naval-battle.instrumentation/around-frame
   [:game/simulation-step 'voxel.world/step-state 3]
   'naval-battle.instrumentation/around-simulation
   [:game/ocean-step 'voxel.ocean/step-ocean 4]
   'naval-battle.instrumentation/around-ocean
   [:game/player-fire 'voxel.world/fire 4]
   'naval-battle.instrumentation/around-fire
   [:game/impact 'voxel.world/apply-impact 2]
   'naval-battle.instrumentation/around-impact})

(deftest exact-real-source-weave-report
  (let [report (edn/read-string
                (slurp (or (System/getenv "NAVAL_ASPECT_REPORT")
                           "../../target/instrumentation/aspects.edn")))
        actual
        (into {}
              (map (fn [aspect]
                     [[(:id aspect)
                       (or (get-in aspect [:match :entry])
                           (get-in aspect [:match :call]))
                       (get-in aspect [:match :arity])]
                      (:advice aspect)]))
              (:aspects report))]
    (testing "all and only the intended unchanged call and entry seams are woven"
      (is (= expected actual))
      (is (= (count expected) (count (:aspects report)))))
    (testing "every manifest expectation resolves to exactly one physical site"
      (is (every? #(= 1 (count (:sites %))) (:aspects report))))
    (testing "the report is an observational, non-control build"
      (is (false? (:control-enabled? report)))
      (is (= "jolt.aspect-ir/v1" (:weaver report))))))
