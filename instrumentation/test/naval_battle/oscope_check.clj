(ns naval-battle.oscope-check
  "Assert the persisted result of the separate-process oscope acceptance run."
  (:require [db.jdbc]
            [jdbc.chdb]
            [jdbc.core :as jdbc]))

(def service "naval-battle-case-study")

(defn- require! [label predicate data]
  (when-not predicate
    (throw (ex-info (str "oscope acceptance failed: " label) data))))

(defn -main [db-spec]
  (let [connection (jdbc/connection db-spec)]
    (try
      (let [spans (jdbc/fetch
                   connection
                   ["select TraceId, SpanId, SpanName from otel_traces
                       where ServiceName=? order by Timestamp" service])
            logs (jdbc/fetch
                  connection
                  ["select TraceId, SpanId, EventName, Body from otel_logs
                      where ServiceName=? order by Timestamp" service])
            sums (jdbc/fetch
                  connection
                  ["select MetricName from otel_metrics_sum
                      where ServiceName=?" service])
            histograms (jdbc/fetch
                        connection
                        ["select MetricName from otel_metrics_histogram
                            where ServiceName=?" service])
            span-names (set (map :spanname spans))
            event-names (set (map :eventname logs))
            metric-names (into (set (map :metricname sums))
                               (map :metricname histograms))
            fire-log (first (filter #(= "game.action.fire" (:eventname %)) logs))]
        (require! "game spans persisted"
                  (every? span-names ["game.match.start" "game.action.fire"])
                  {:span-names span-names})
        (require! "game event logs persisted"
                  (every? event-names ["game.match.start" "game.action.fire"])
                  {:event-names event-names})
        (require! "fire log is correlated to its active span"
                  (and (seq (:traceid fire-log)) (seq (:spanid fire-log)))
                  {:fire-log fire-log})
        (require! "game metrics persisted"
                  (every? metric-names
                          ["io.github.casselc.game_engine.operation.duration"
                           "io.github.casselc.game.actions"])
                  {:metric-names metric-names})
        (prn {:service service
              :spans span-names
              :events event-names
              :metrics metric-names
              :correlated-fire-log true}))
      (finally (.close connection)))))
