(ns naval-battle.instrumentation-test
  (:require [clojure.test :refer [deftest is testing]]
            [naval-battle.instrumentation :as inst]
            [otel.exporter.memory :as memory]
            [otel.sdk :as sdk]))

(deftest vocabulary-is-closed-and-privacy-preserving
  (testing "known dimensions are finite"
    (is (= #{"input" "physics" "simulation" "ocean" "render"}
           inst/operation-names))
    (is (= #{"player" "enemy" "_OTHER"} inst/player-roles))
    (is (= #{"fired" "not_fired"} inst/action-outcomes))
    (is (= #{"playing" "over" "_OTHER"} inst/game-phases)))
  (testing "arbitrary identifiers cannot become telemetry values"
    (doseq [value [nil true false 0 42 "captain@example.test"
                   "10.0.0.1" :spectator [:player] {:id :player}]]
      (is (= "_OTHER" (inst/safe-player-role value)))
      (is (= "_OTHER" (inst/safe-game-phase value))))))

(deftest fire-advice-preserves-result-identity-and-records-bounded-fields
  (let [exporter (memory/multisignal-exporter)
        handle (sdk/init! {:service-name "naval-battle-test"
                           :exporter exporter :processor :simple
                           :metrics? true :runtime-metrics? false})
        state {:phase :playing :shells []}
        result {:phase :playing :shells [{:secret-target [12 34 56]}]}
        calls (atom 0)]
    (try
      (is (identical? result
                      (inst/around-fire
                       {} [state :player [12 34 56] 16.0]
                       #(do (swap! calls inc) result))))
      (is (= 1 @calls))
      (is (sdk/force-flush! handle))
      (let [span (first (memory/spans exporter))
            attrs (:attributes span)]
        (is (= "game.action.fire" (:name span)))
        (is (= "player"
               (get attrs "io.github.casselc.game.ship.side")))
        (is (= "fired"
               (get attrs "io.github.casselc.game.action.outcome")))
        (is (not-any? #(re-find #"target|position|email|address"
                                (str (key %)))
                      attrs))
        (is (not-any? #(= [12 34 56] (val %)) attrs)))
      (finally
        (sdk/shutdown! handle)))))

(deftest observation-does-not-replace-application-exceptions
  (let [error (ex-info "application-owned" {:private "do-not-record"})]
    (try
      (inst/around-simulation {} [{:phase :playing} 0.01 {:bodies []}]
                              #(throw error))
      (is false "expected application exception")
      (catch Throwable actual
        (is (identical? error actual))))))

(deftest aspect-provider-covers-only-the-declared-roles
  (is (= #{:game/input :game/session :game/physics :game/render :game/frame
           :game/simulation :game/ocean :game/action :game/impact}
         (set (keys (:roles inst/aspect-provider)))))
  (is (= inst/target-revision
         (get-in inst/aspect-provider [:libraries 'casselc/naval-battle]))))
