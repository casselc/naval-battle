(ns naval-battle.instrumentation
  "Fork-local OpenTelemetry advice for the naval-battle case study.

  The provider observes only closed game vocabulary. It never retains cursor
  coordinates, aim targets, ship positions, voxel cells, native handles, or
  exception messages. The compiler owns result/exception preservation and
  fails open if this observational code itself fails."
  (:require [jolt.host :as host]
            [otel.context :as context]
            [otel.logs :as logs]
            [otel.metrics :as metrics]
            [otel.sdk :as sdk]
            [otel.trace :as trace]))

(def target-revision
  "Exact unchanged gameplay revision described by the fork-local manifest."
  "6839a803fca573d3c65ff9df2bb9afbf10fa270b")

(def ^:private instrumentation-version "0.1.0")
(def ^:private scope-name "io.github.casselc.naval-battle.auto")

(def operation-names
  "Complete low-cardinality domain of hot-loop operation labels."
  #{"input" "physics" "simulation" "ocean" "render"})

(def player-roles
  "Complete low-cardinality domain of player-role labels."
  #{"player" "enemy" "_OTHER"})

(def action-outcomes
  "Complete low-cardinality domain of fire outcomes."
  #{"fired"})

(def game-phases
  "Complete low-cardinality domain of game phases retained as telemetry."
  #{"playing" "over" "_OTHER"})

(defn safe-player-role [value]
  (case value
    :player "player"
    :enemy "enemy"
    "_OTHER"))

(defn safe-game-phase [value]
  (case value
    :playing "playing"
    :over "over"
    "_OTHER"))

(defonce ^:private instruments* (atom {:provider ::unset}))

(defn- emit-event! [event-name body attributes]
  (try
    (logs/emit! (sdk/logger scope-name {:version instrumentation-version})
                {:event-name event-name
                 :body body
                 :severity :info
                 :attributes attributes})
    (catch :default _ nil)))

(defn- instruments []
  (let [provider (sdk/meter-provider)]
    (locking instruments*
      (if (identical? provider (:provider @instruments*))
        @instruments*
        (let [meter (sdk/meter scope-name {:version instrumentation-version})
              next {:provider provider
                    :duration
                    (metrics/histogram
                     meter "io.github.casselc.game_engine.operation.duration"
                     {:description "Duration of one game engine operation."
                      :unit "s"
                      :boundaries [0.0001 0.0005 0.001 0.0025 0.005
                                   0.01 0.025 0.05 0.1]})
                    :actions
                    (metrics/counter
                     meter "io.github.casselc.game.actions"
                     {:description "Number of accepted player actions."
                      :unit "{action}"})
                    :frames
                    (metrics/counter
                     meter "io.github.casselc.game_engine.frames"
                     {:description "Number of frames presented."
                      :unit "{frame}"})}]
          (reset! instruments* next)
          next)))))

(defn- record-duration! [operation elapsed phase]
  (when (and elapsed (contains? operation-names operation))
    (try
      (metrics/record! (:duration (instruments))
                       (/ elapsed 1000000000.0)
                       (cond-> {:io.github.casselc.game_engine.operation.name
                                operation}
                         phase (assoc :io.github.casselc.game.match.phase
                                      phase)))
      (catch :default _ nil))))

(defn- around-operation [operation phase proceed]
  (if (context/instrumentation-suppressed?)
    (proceed)
    (let [start (try (host/mono-nanos) (catch :default _ nil))]
      (try
        (proceed)
        (finally
          (let [end (try (host/mono-nanos) (catch :default _ nil))]
            (record-duration! operation
                              (when (and start end) (max 0 (- end start)))
                              phase)))))))

(defn around-input [_join-point _args proceed]
  (around-operation "input" nil proceed))

(defn around-physics [_join-point _args proceed]
  (around-operation "physics" nil proceed))

(defn around-render [_join-point [frame] proceed]
  (around-operation "render"
                    (safe-game-phase (get-in frame [:world :phase]))
                    proceed))

(defn around-frame [_join-point _args proceed]
  (let [result (proceed)]
    (try (metrics/add! (:frames (instruments)) 1) (catch :default _ nil))
    result))

(defn around-ocean [_join-point _args proceed]
  (around-operation "ocean" nil proceed))

(defn around-simulation [_join-point [state _dt _facts] proceed]
  (let [before (:phase state)
        result (around-operation "simulation" (safe-game-phase before) proceed)]
    (when (and (= :playing before) (= :over (:phase result)))
      (let [span (trace/start-span
                  (sdk/tracer scope-name {:version instrumentation-version})
                  "game.match.end"
                  {:kind :internal
                   :attributes {:io.github.casselc.game.match.phase "over"
                                :io.github.casselc.game.ship.side
                                (safe-player-role (:winner result))}})]
        (try
          (trace/with-current-span span
            (emit-event! "game.match.end" "match ended"
                         {:io.github.casselc.game.match.phase "over"
                          :io.github.casselc.game.ship.side
                          (safe-player-role (:winner result))}))
          (finally (trace/end! span)))))
    result))

(defn around-session [_join-point _args proceed]
  (let [result (proceed)
        span (trace/start-span
              (sdk/tracer scope-name {:version instrumentation-version})
              "game.match.start"
              {:kind :internal
               :attributes
               {:io.github.casselc.game.match.phase
                (safe-game-phase (:phase result))}})]
    (try
      (trace/with-current-span span
        (emit-event! "game.match.start" "match started"
                     {:io.github.casselc.game.match.phase
                      (safe-game-phase (:phase result))}))
      (finally (trace/end! span)))
    result))

(defn- safe-impact [value]
  (case value
    :hit "hit"
    :splash "splash"
    "_OTHER"))

(defn around-impact [_join-point [_state impact] proceed]
  (let [kind (safe-impact (:kind impact))
        span (trace/start-span
              (sdk/tracer scope-name {:version instrumentation-version})
              "game.impact"
              {:kind :internal
               :attributes
               (cond-> {:io.github.casselc.game.combat.outcome kind}
                             (= "hit" kind)
                             (assoc :io.github.casselc.game.ship.side
                                    (safe-player-role (:ship impact))))})]
    (try
      (trace/with-current-span span
        (let [result (proceed)]
          (emit-event! "game.impact" "impact observed"
                       (cond-> {:io.github.casselc.game.combat.outcome kind}
                         (= "hit" kind)
                         (assoc :io.github.casselc.game.ship.side
                                (safe-player-role (:ship impact)))))
          result))
      (catch :default error
        (try (trace/set-status! span :error) (catch :default _ nil))
        (throw error))
      (finally (try (trace/end! span) (catch :default _ nil))))))

(defn around-fire
  "Trace an accepted shot without retaining the target or any world entity id.

  Cooldown and other rejected no-op calls are deliberately silent: the enemy
  invokes this seam from the frame loop, so tracing every attempt would turn a
  sparse domain signal into frame-rate telemetry."
  [_join-point [state role & _target-and-speed] proceed]
  (if (context/instrumentation-suppressed?)
    (proceed)
    (let [role (safe-player-role role)
          before (count (:shells state))
          result (proceed)]
      (when (> (count (:shells result)) before)
        (let [attrs {:io.github.casselc.game.action.name "fire"
                     :io.github.casselc.game.action.outcome "fired"
                     :io.github.casselc.game.ship.side role}
              span (trace/start-span
                    (sdk/tracer scope-name
                                {:version instrumentation-version})
                    "game.action.fire"
                    {:kind :internal
                     :attributes
                     (assoc attrs :io.github.casselc.game.match.phase
                            (safe-game-phase (:phase state)))})]
          (try
            (trace/with-current-span span
              (try
                (metrics/add! (:actions (instruments)) 1 attrs)
                (emit-event! "game.action.fire" "shot fired" attrs)
                (catch :default _ nil)))
            (finally
              (try (trace/end! span) (catch :default _ nil))))))
      result)))

(def aspect-provider
  {:schema 1
   :libraries {'casselc/naval-battle target-revision}
   :roles {:game/input {:fn 'naval-battle.instrumentation/around-input
                        :contract :args-v1}
           :game/session {:fn 'naval-battle.instrumentation/around-session
                          :contract :args-v1}
           :game/physics {:fn 'naval-battle.instrumentation/around-physics
                          :contract :args-v1}
           :game/render {:fn 'naval-battle.instrumentation/around-render
                         :contract :args-v1}
           :game/simulation {:fn 'naval-battle.instrumentation/around-simulation
                             :contract :args-v1}
           :game/ocean {:fn 'naval-battle.instrumentation/around-ocean
                        :contract :args-v1}
           :game/frame {:fn 'naval-battle.instrumentation/around-frame
                        :contract :args-v1}
           :game/action {:fn 'naval-battle.instrumentation/around-fire
                         :contract :args-v1}
           :game/impact {:fn 'naval-battle.instrumentation/around-impact
                         :contract :args-v1}}})
