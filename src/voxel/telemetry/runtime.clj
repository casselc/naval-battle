(ns voxel.telemetry.runtime
  "One owner for naval-battle's opt-in Durable SDK and adjacent oscope viewer."
  (:require [jdbc.chdb.durable.local-posix :as local-posix]
            [oscope.embedded :as embedded]
            [voxel.telemetry.config :as config]
            [voxel.telemetry.hud :as hud]
            [voxel.telemetry.viewer :as viewer])
  (:import [java.util UUID]))

(defn- db-spec [{:keys [storage-root object-id owner database]}]
  {:vendor "chdb-durable"
   :namespace-backend (local-posix/local-backend storage-root)
   :object-id object-id
   :owner owner
   :instance (str (UUID/randomUUID))
   :database database})

(defn- stop-result [state error]
  (cond-> {:status (if (= :closed (:phase state)) :closed :closing)
           :phase (:phase state)}
    error (assoc :errors [error])))

(defn- stop-embedded-until-closed! [runtime]
  (loop [attempt 1]
    (let [result (embedded/stop! runtime)]
      (cond
        (= :closed (:status result)) result
        (< attempt 4) (recur (inc attempt))
        :else result))))

(defn- stop-lifecycle! [{:keys [viewer hud embedded state lock]}]
  (locking lock
    (if (= :closed (:phase @state))
      (stop-result @state nil)
      (try
        ;; Stop human query ingress before the source/connection it reads.
        (when-not (:viewer-stopped? @state)
          (viewer/stop! viewer)
          (swap! state assoc :viewer-stopped? true :phase :stopping-hud))
        (when-not (:hud-stopped? @state)
          (hud/stop! hud)
          (swap! state assoc :hud-stopped? true :phase :stopping-embedded))
        (when-not (:embedded-stopped? @state)
          (let [result (embedded/stop! embedded)]
            (when-not (= :closed (:status result))
              (throw (ex-info "embedded telemetry did not finish shutdown"
                              {:voxel.telemetry/error true
                               :type ::embedded-shutdown-incomplete
                               :result result})))
            (swap! state assoc :embedded-stopped? true :phase :closed)))
        (stop-result @state nil)
        (catch Throwable error
          (stop-result @state error))))))

(defn start!
  "Start Durable collection and a viewer without changing the game lifecycle."
  [options]
  (let [options (config/normalize options)
        embedded* (atom nil)
        hud* (atom nil)]
    (try
      (let [embedded-runtime
            (embedded/start!
             {:db-spec (db-spec options)
              :checkpoint-on-close? (:checkpoint-on-close? options)
              :sdk-options (:sdk-options options)})
            _ (reset! embedded* embedded-runtime)
            hud-runtime (hud/start! (:source embedded-runtime)
                                    {:interval-ms (:hud-interval-ms options)})
            _ (reset! hud* hud-runtime)
            viewer-runtime
            (viewer/start! {:source (:source embedded-runtime)
                            :connection (:connection embedded-runtime)
                            :host (:host options)
                            :port (:port options)})
            lifecycle {:options options
                       :embedded embedded-runtime
                       :hud hud-runtime
                       :viewer viewer-runtime
                       :source (:source embedded-runtime)
                       :connection (:connection embedded-runtime)
                       :state (atom {:phase :open
                                     :viewer-stopped? false
                                     :hud-stopped? false
                                     :embedded-stopped? false})
                       :lock (Object.)}]
        (assoc lifecycle :stop! #(stop-lifecycle! lifecycle)))
      (catch Throwable error
        (when-let [sampler @hud*]
          (try (hud/stop! sampler) (catch Throwable _ nil)))
        (when-let [runtime @embedded*]
          (try (stop-embedded-until-closed! runtime) (catch Throwable _ nil)))
        (throw error)))))

(defn force-flush! [lifecycle]
  (embedded/force-flush! (:embedded lifecycle)))

(defn stop! [lifecycle]
  (if-let [stop-fn (:stop! lifecycle)]
    (stop-fn)
    (throw (ex-info "invalid naval-battle telemetry lifecycle"
                    {:voxel.telemetry/error true :type ::invalid-lifecycle}))))

(defn stop-until-closed!
  "Retry the explicit, idempotent Durable shutdown boundaries a bounded number
  of times, then surface the retained phase and errors."
  [lifecycle]
  (loop [attempt 1]
    (let [result (stop! lifecycle)]
      (cond
        (= :closed (:status result)) result
        (< attempt 4) (recur (inc attempt))
        :else (throw (ex-info "naval-battle telemetry shutdown remained incomplete"
                             {:voxel.telemetry/error true
                              :type ::shutdown-incomplete
                              :result result}))))))
