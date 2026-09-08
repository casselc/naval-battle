(ns naval-battle.embedded-check
  "Assert game-generated telemetry after an independent Durable reopen."
  (:require [db.jdbc]
            [jdbc.chdb]
            [jdbc.chdb.durable]
            [jdbc.chdb.durable.local-posix :as local-posix]
            [jdbc.core :as jdbc]))

(defn- require! [label predicate data]
  (when-not predicate
    (throw (ex-info (str "embedded native acceptance failed: " label) data))))

(defn -main [storage-root object-id service]
  (with-open [connection
              (jdbc/connection
               {:vendor "chdb-durable"
                :namespace-backend (local-posix/local-backend storage-root)
                :object-id object-id
                :read-only? true})]
    (let [spans (jdbc/fetch
                 connection
                 ["select SpanName as name, count() as n from otel_traces
                     where ServiceName=? group by SpanName order by name" service])
          logs (jdbc/fetch
                connection
                ["select EventName as name, count() as n from otel_logs
                    where ServiceName=? group by EventName order by name" service])
          sums (jdbc/fetch
                connection
                ["select MetricName as name, sum(Value) as value
                    from otel_metrics_sum where ServiceName=?
                    group by MetricName order by name" service])
          histograms
          (jdbc/fetch
           connection
           ["select MetricName as name, sum(Count) as observations
               from otel_metrics_histogram where ServiceName=?
               group by MetricName order by name" service])
          span-names (set (map :name spans))
          log-names (set (map :name logs))
          sum-values (into {} (map (juxt :name :value)) sums)
          histogram-values
          (into {} (map (juxt :name :observations)) histograms)]
      (require! "match start span persisted"
                (contains? span-names "game.match.start") {:spans spans})
      (require! "match start log persisted"
                (contains? log-names "game.match.start") {:logs logs})
      (require! "frame advice recorded real presentations"
                (pos? (or (get sum-values
                               "io.github.casselc.game_engine.frames") 0))
                {:sums sums})
      (require! "operation advice recorded real engine work"
                (pos? (or (get histogram-values
                               "io.github.casselc.game_engine.operation.duration")
                          0))
                {:histograms histograms})
      (prn {:service service
            :spans span-names
            :logs log-names
            :frames (get sum-values "io.github.casselc.game_engine.frames")
            :operation-observations
            (get histogram-values
                 "io.github.casselc.game_engine.operation.duration")}))))
