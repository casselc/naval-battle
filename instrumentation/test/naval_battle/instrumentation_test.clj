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
    (is (= #{"fired"} inst/action-outcomes))
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
  (let [exporter (memory/multisignal-exporter)
        handle (sdk/init! {:service-name "naval-battle-test"
                           :exporter exporter :processor :simple
                           :metrics? true :runtime-metrics? false})
        error (ex-info "captain@example.test" {:private "secret-target"})]
    (try
      (try
        (inst/around-simulation {} [{:phase :playing} 0.01 {:bodies []}]
                                #(throw error))
        (is false "expected application exception")
        (catch Throwable actual
          (is (identical? error actual))))
      (try
        (inst/around-fire
         {} [{:phase :playing :shells []}
             :player ["captain@example.test" "secret-target"] 16.0]
         #(throw error))
        (is false "expected application exception")
        (catch Throwable actual
          (is (identical? error actual))))
      (let [spans (memory/spans exporter)
            logs (memory/records exporter)
            metrics (memory/metrics exporter)
            exported (pr-str {:spans spans :logs logs :metrics metrics})]
        (is (empty? spans))
        (is (empty? logs))
        (is (empty? metrics))
        (is (not (re-find #"captain@example|secret-target" exported))))
      (finally
        (sdk/shutdown! handle)))))

(deftest rejected-fire-is-silent
  (let [exporter (memory/multisignal-exporter)
        handle (sdk/init! {:service-name "naval-battle-test"
                           :exporter exporter :processor :simple
                           :metrics? true :runtime-metrics? false})
        state {:phase :playing :shells []}]
    (try
      (is (identical? state
                      (inst/around-fire
                       {} [state :enemy [12 34 56] 16.0]
                       #(identity state))))
      (is (empty? (memory/spans exporter)))
      (is (empty? (memory/records exporter)))
      (is (empty? (memory/metrics exporter)))
      (finally
        (sdk/shutdown! handle)))))

(deftest aspect-provider-covers-only-the-declared-roles
  (is (= #{:game/input :game/session :game/physics :game/render :game/frame
           :game/simulation :game/ocean :game/action :game/impact}
         (set (keys (:roles inst/aspect-provider)))))
  (is (= inst/target-revision
         (get-in inst/aspect-provider [:libraries 'casselc/naval-battle]))))

(deftest frame-advice-draws-before-present-and-preserves-identities
  (let [events (atom [])
        result (Object.)]
    (with-redefs [inst/draw-hud-fail-open! #(swap! events conj :draw)]
      (is (identical? result
                      (inst/around-frame
                       {} [] #(do (swap! events conj :present) result))))
      (is (= [:draw :present] @events)))
    (let [error (ex-info "application present failed" {:private true})]
      (with-redefs [inst/draw-hud-fail-open! (constantly nil)]
        (try
          (inst/around-frame {} [] #(throw error))
          (is false "expected application exception")
          (catch Throwable actual
            (is (identical? error actual))))))))

(deftest hud-drawing-fails-open-before-present
  (let [calls (atom 0)
        result (Object.)]
    (with-redefs [inst/draw-hud-fail-open!
                  #(inst/draw-hud-with-resolver-fail-open!
                    (fn [] #(throw (ex-info "draw failed" {}))))]
      (is (identical? result
                      (inst/around-frame
                       {} [] #(do (swap! calls inc) result))))
      (is (= 1 @calls))))
  (testing "a profile without the optional HUD namespace still presents"
    (let [calls (atom 0)]
      (with-redefs [inst/draw-hud-fail-open!
                    #(inst/draw-hud-with-resolver-fail-open!
                      (fn [] (throw (ex-info "not on classpath" {}))))]
        (inst/around-frame {} [] #(swap! calls inc))
        (is (= 1 @calls))))))
