(ns voxel.telemetry.hud
  "Background oscope sampling for render-safe advice.

  The worker is the only code in this namespace that invokes the oscope query
  command. `snapshot` only dereferences an atom, so end-of-frame advice can draw
  the cached immutable value without blocking the raylib thread on chDB."
  (:require [jolt.fibers :as fibers])
  (:import [java.util.concurrent Executors TimeUnit]))

(def span-selection
  {:signal :spans :field :span-name :window :15m :limit 6})

(def metric-selection
  {:signal :metrics :field :metric-name :window :15m :limit 6})

(def ^:private empty-model
  {:status :starting :sampled-at-unix-ms nil :spans [] :metrics []})

(defonce ^:private active* (atom nil))

(defn- values [screen]
  (mapv #(select-keys % [:value :count]) (get-in screen [:table :rows])))

(defn load-model
  "Run the two fixed, bounded queries used by the compact in-game HUD model."
  [source now-ms]
  (let [load-command (:load-command source)]
    (when-not (ifn? load-command)
      (throw (ex-info "HUD sampler requires an oscope load command"
                      {:voxel.telemetry/error true :type ::invalid-source})))
    {:status :ready
     :sampled-at-unix-ms now-ms
     :spans (values (load-command [:naval-hud :spans now-ms] span-selection))
     :metrics (values (load-command [:naval-hud :metrics now-ms]
                                    metric-selection))}))

(defn- query-on-thread!
  "Run the JDBC/chDB section on its one owned OS thread.

  Durable JDBC currently holds a counted connection lock while it waits on its
  serialized writer. A fiber is correctly forbidden from parking at that point,
  so the cadence fiber parks on this promise while the blocking query remains
  on the dedicated thread."
  [executor load-model-fn]
  (let [result (promise)]
    (.execute executor
              (fn []
                (deliver result
                         (try {:value (load-model-fn)}
                              (catch Throwable error {:error error})))))
    (let [{:keys [value error]} @result]
      (if error (throw error) value))))

(defn- refresh! [model executor load-model-fn]
  (let [prior @model]
    (try
      (reset! model (query-on-thread! executor load-model-fn))
      (catch Throwable error
        ;; Keep the last good rows visible. Error text is deliberately omitted
        ;; from the render model; the UI needs a state, not backend internals.
        (reset! model (assoc prior :status :stale
                             :last-error-class (str (class error))))))))

(defn start!
  "Start one bounded-cadence sampler and publish it as the active HUD.

  `load-model-fn` is injectable for headless tests; production always uses the
  fixed selections above. A fiber owns cadence and cache publication; one
  dedicated thread owns blocking JDBC because Durable's connection lock cannot
  cross a fiber parking boundary."
  ([source] (start! source {}))
  ([source {:keys [interval-ms load-model-fn]
            :or {interval-ms 1000}}]
   (when-not (and (integer? interval-ms) (<= 1 interval-ms 60000))
     (throw (ex-info "HUD interval must be between 1 and 60000 milliseconds"
                     {:voxel.telemetry/error true :type ::invalid-interval
                      :interval-ms interval-ms})))
   (when @active*
     (throw (ex-info "a naval-battle telemetry HUD is already active"
                     {:voxel.telemetry/error true :type ::already-active})))
   (let [model (atom empty-model)
         stop-signal (promise)
         executor (Executors/newSingleThreadExecutor)
         load-model-fn (or load-model-fn
                           #(load-model source (System/currentTimeMillis)))
         worker (fibers/spawn
                 (fn []
                   (loop []
                     (when (= ::tick (deref stop-signal interval-ms ::tick))
                       (refresh! model executor load-model-fn)
                       (recur)))
                   :stopped))
         sampler {:model model :stop-signal stop-signal :worker worker
                  :executor executor :interval-ms interval-ms
                  :stopped? (atom false)}]
     (if (compare-and-set! active* nil sampler)
       sampler
       (do
         (deliver stop-signal true)
         (fibers/join worker)
         (.shutdownNow executor)
         (throw (ex-info "a naval-battle telemetry HUD is already active"
                         {:voxel.telemetry/error true :type ::already-active})))))))

(defn snapshot
  "Return the cached immutable model. This function never queries chDB."
  ([] (if-let [sampler @active*] @(:model sampler) empty-model))
  ([sampler] @(:model sampler)))

(defn stop!
  "Signal the worker and wait for any bounded in-flight query to finish."
  [sampler]
  (when (compare-and-set! (:stopped? sampler) false true)
    (deliver (:stop-signal sampler) true)
    (let [result (fibers/join (:worker sampler) 7000 ::timeout)]
      (when (= ::timeout result)
        (reset! (:stopped? sampler) false)
        (throw (ex-info "HUD query worker did not stop within its bound"
                        {:voxel.telemetry/error true
                         :type ::stop-timeout})))
      (.shutdown (:executor sampler))
      (when-not (.awaitTermination (:executor sampler) 1000
                                   TimeUnit/MILLISECONDS)
        (.shutdownNow (:executor sampler)))
      (compare-and-set! active* sampler nil)))
  true)
