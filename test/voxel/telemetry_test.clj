(ns voxel.telemetry-test
  (:require [clojure.test :refer [deftest is testing]]
            [oscope.embedded :as embedded]
            [oscope.sample :as sample]
            [voxel.telemetry.config :as config]
            [voxel.telemetry.hud :as hud]
            [voxel.telemetry.hud-overlay :as overlay]
            [voxel.telemetry.runtime :as runtime]
            [voxel.telemetry.viewer :as viewer]))

(deftest viewer-has-no-otlp-ingress-and-keeps-queries-bounded
  (let [seen (atom nil)
        source {:load-command
                (fn [_ selection]
                  (reset! seen selection)
                  (sample/screen-for-selection selection))
                :export-admission {:capacity 1 :active (atom 0)}}
        app (viewer/handler
             {:authority "127.0.0.1:4320"
              :workbench-handler (constantly {:status 200 :body "traces"})
              :events-handler (constantly {:status 200 :body "events"})
              :aggregate-handler
              ((requiring-resolve 'oscope.ui.web/handler) source)
              :editor-handler (constantly {:status 200 :body "editor"})})
        request {:request-method :get
                 :headers {"Host" "127.0.0.1:4320"}}]
    (testing "collector endpoints do not exist in the viewer-only router"
      (is (= 404 (:status (app (assoc request :uri "/v1/traces")))))
      (is (= 404 (:status (app (assoc request :uri "/v1/logs")))))
      (is (= 404 (:status (app (assoc request :uri "/v1/metrics"))))))
    (testing "oversized query limits fall back inside oscope's bounded policy"
      (is (= 200 (:status
                  (app (assoc request :uri "/oscope"
                              :query-string "signal=spans&field=span-name&limit=999")))))
      (is (<= (:limit @seen) 100)))
    (testing "DNS rebinding protection is retained"
      (is (= 421 (:status
                  (app (assoc request :uri "/healthz"
                              :headers {"Host" "game.example"}))))))))

(deftest viewer-stop-remains-retryable-after-a-transient-failure
  (let [attempts (atom 0)
        source {:load-command
                (fn [_ selection] (sample/screen-for-selection selection))
                :export-admission {:capacity 1 :active (atom 0)}}]
    (with-redefs [jolt.http.server/run-server
                  (fn [& _] {:port 4320})
                  jolt.http.server/stop-server
                  (fn [_]
                    (when (= 1 (swap! attempts inc))
                      (throw (ex-info "transient listener stop failure" {})))
                    true)]
      (let [runtime (viewer/start! {:source source
                                    :connection ::connection
                                    :host "127.0.0.1"
                                    :port 4320})]
        (is (thrown? Exception (viewer/stop! runtime)))
        (is (false? @(:stopped? runtime)))
        (is (true? (viewer/stop! runtime)))
        (is (true? @(:stopped? runtime)))
        (is (= 2 @attempts))
        (is (true? (viewer/stop! runtime)))
        (is (= 2 @attempts))))))

(deftest composition-validates-before-effects-and-stops-viewer-first
  (let [events (atom [])
        embedded-runtime {:source ::source :connection ::connection}
        viewer-runtime {:url "http://127.0.0.1:4320/oscope/telemetry"}]
    (with-redefs [jdbc.chdb.durable.local-posix/local-backend
                  (fn [root]
                    (swap! events conj [:storage root])
                    ::backend)
                  embedded/start!
                  (fn [options]
                    (swap! events conj [:embedded-start options])
                    embedded-runtime)
                  hud/start!
                  (fn [source options]
                    (is (= ::source source))
                    (swap! events conj [:hud-start options])
                    ::hud)
                  viewer/start!
                  (fn [options]
                    (swap! events conj [:viewer-start options])
                    viewer-runtime)
                  viewer/stop!
                  (fn [actual]
                    (is (= viewer-runtime actual))
                    (swap! events conj :viewer-stop)
                    true)
                  hud/stop!
                  (fn [actual]
                    (is (= ::hud actual))
                    (swap! events conj :hud-stop)
                    true)
                  embedded/stop!
                  (fn [actual]
                    (is (= embedded-runtime actual))
                    (swap! events conj :embedded-stop)
                    {:status :closed :phase :closed})]
      (is (thrown? Exception
                   (runtime/start! {:host "0.0.0.0"})))
      (is (empty? @events))
      (let [lifecycle (runtime/start! {:storage-root "./telemetry-test"
                                       :port 4320})]
        (is (= {:status :closed :phase :closed}
               (runtime/stop-until-closed! lifecycle)))
        (is (= {:status :closed :phase :closed}
               (runtime/stop! lifecycle)))
        (is (= :viewer-stop (nth @events 4)))
        (is (= :hud-stop (nth @events 5)))
        (is (= :embedded-stop (nth @events 6)))))))

(deftest failed-viewer-start-closes-the-embedded-runtime
  (let [events (atom [])
        embedded-runtime {:source ::source :connection ::connection}]
    (with-redefs [jdbc.chdb.durable.local-posix/local-backend (constantly ::backend)
                  embedded/start! (fn [_]
                                    (swap! events conj :embedded-start)
                                    embedded-runtime)
                  hud/start! (fn [_ _]
                               (swap! events conj :hud-start)
                               ::hud)
                  hud/stop! (fn [_]
                              (swap! events conj :hud-stop)
                              true)
                  viewer/start! (fn [_]
                                  (swap! events conj :viewer-fail)
                                  (throw (ex-info "bind failed" {})))
                  embedded/stop! (fn [_]
                                   (swap! events conj :embedded-stop)
                                   {:status :closed :phase :closed})]
      (is (thrown? Exception (runtime/start! {})))
      (is (= [:embedded-start :hud-start :viewer-fail :hud-stop :embedded-stop]
             @events)))))

(deftest failed-start-surfaces-an-incomplete-embedded-rollback
  (let [startup-error (ex-info "bind failed" {:stage :viewer})
        persistence-error (ex-info "checkpoint failed" {:stage :persistence})
        stop-attempts (atom 0)
        embedded-runtime {:source ::source :connection ::connection}]
    (with-redefs [jdbc.chdb.durable.local-posix/local-backend (constantly ::backend)
                  embedded/start! (constantly embedded-runtime)
                  hud/start! (fn [_ _] ::hud)
                  hud/stop! (constantly true)
                  viewer/start! (fn [_] (throw startup-error))
                  embedded/stop!
                  (fn [_]
                    (swap! stop-attempts inc)
                    {:status :closing :phase :persisting
                     :errors [persistence-error]})]
      (try
        (runtime/start! {})
        (is false "expected startup rollback failure")
        (catch Throwable error
          (let [data (ex-data error)
                cleanup (first (:cleanup-errors data))]
            (is (= ::runtime/startup-and-cleanup-failed (:type data)))
            (is (identical? startup-error (:startup-error data)))
            (is (= ::runtime/startup-rollback-incomplete
                   (:type (ex-data cleanup))))
            (is (identical? persistence-error
                            (first (get-in (ex-data cleanup)
                                           [:result :errors])))))))
      (is (= 4 @stop-attempts)))))

(deftest hud-queries-only-on-its-worker-and-snapshot-is-memory-only
  (let [calls (atom 0)
        sampler (hud/start!
                 ::source
                 {:interval-ms 1
                  :load-model-fn
                  #(do (swap! calls inc)
                       {:status :ready :sampled-at-unix-ms 1
                        :spans [{:value "game.frame" :count 2}]
                        :metrics []})})]
    (try
      (let [first-read
            (loop [remaining 100]
              (let [model (hud/snapshot sampler)]
                (if (or (= :ready (:status model)) (zero? remaining))
                  model
                  (do (Thread/sleep 10) (recur (dec remaining))))))
            _ (is (true? (hud/stop! sampler)))
            before @calls
            second-read (hud/snapshot sampler)]
        (is (= :ready (:status first-read)))
        (is (= first-read second-read))
        (is (= before @calls)
            "snapshot does not invoke the query function"))
      (finally
        (is (true? (hud/stop! sampler)))))))

(deftest hud-model-uses-only-fixed-small-queries
  (let [selections (atom [])
        source {:load-command
                (fn [_ selection]
                  (swap! selections conj selection)
                  {:table {:rows [{:value "sample" :count 1}]}})}
        model (hud/load-model source 42)]
    (is (= :ready (:status model)))
    (is (= 42 (:sampled-at-unix-ms model)))
    (is (= [hud/span-selection hud/metric-selection] @selections))
    (is (every? #(= 6 (:limit %)) @selections))))

(deftest hud-overlay-reads-one-snapshot-and-draws-a-bounded-model
  (let [snapshots (atom 0)
        calls (atom [])
        rows (mapv (fn [n] {:value (str "row-" n) :count n}) (range 20))]
    (with-redefs [hud/snapshot
                  (fn []
                    (swap! snapshots inc)
                    {:status :ready :spans rows :metrics rows})
                  voxel.raylib/draw-rectangle
                  #(swap! calls conj [:background %1 %2 %3 %4 %5])
                  voxel.raylib/draw-rectangle-lines
                  #(swap! calls conj [:border %1 %2 %3 %4 %5])
                  voxel.raylib/draw-text
                  #(swap! calls conj [:text %1 %2 %3 %4 %5])]
      (is (nil? (overlay/draw-current!)))
      (is (= 1 @snapshots))
      (is (= 11 (count @calls)))
      (is (= 9 (count (filter #(= :text (first %)) @calls)))))))

(deftest hud-line-model-is-bounded-for-arbitrary-shaped-rows
  ;; A deterministic property sweep covers absent, malformed, oversized, and
  ;; newline-bearing values without adding a native generator to this profile.
  (doseq [status [:ready :stale :starting :unknown nil]
          row-count [0 1 3 4 20]
          value [nil "" "normal" "line\nbreak" (apply str (repeat 100 "x"))]
          count-value [nil -7 0 12 1000000000000 "many"]]
    (let [rows (vec (repeat row-count {:value value :count count-value}))
          lines (overlay/model-lines {:status status
                                      :spans rows
                                      :metrics rows})]
      (is (<= (count lines) 9))
      (is (every? #(not (re-find #"[\r\n\t]" %)) lines))
      (is (every? #(<= (count %) 45) lines)))))

(deftest inactive-hud-does-not-issue-draw-calls
  (with-redefs [hud/snapshot (constantly nil)
                voxel.raylib/draw-rectangle
                (fn [& _] (throw (ex-info "unexpected draw" {})))
                voxel.raylib/draw-rectangle-lines
                (fn [& _] (throw (ex-info "unexpected draw" {})))
                voxel.raylib/draw-text
                (fn [& _] (throw (ex-info "unexpected draw" {})))]
    (is (nil? (overlay/draw-current!)))))

(deftest environment-configuration-is-bounded-and-explicit
  (let [env {"VOXEL_OTEL_VIEWER_PORT" "0"
             "VOXEL_OTEL_STORAGE_ROOT" "/tmp/naval-telemetry"
             "VOXEL_OTEL_OBJECT_ID" "case-study"
             "OTEL_SERVICE_NAME" "naval-battle-acceptance"}
        options (config/env-options env)]
    (is (= 0 (:port options)))
    (is (= "/tmp/naval-telemetry" (:storage-root options)))
    (is (= "case-study" (:object-id options)))
    (is (= "naval-battle-acceptance"
           (get-in options [:sdk-options :service-name])))
    (is (= "127.0.0.1" (:host options))))
  (is (thrown? Exception
               (config/env-options {"VOXEL_OTEL_VIEWER_PORT" "70000"}))))
