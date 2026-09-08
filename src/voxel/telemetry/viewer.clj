(ns voxel.telemetry.viewer
  "Viewer-only loopback surface over an existing oscope embedded source.

  This namespace deliberately has no OTLP receiver route. Game telemetry reaches
  chDB through the in-process SDK/exporter; HTTP exists only for human queries."
  (:require [clojure.string :as str]
            [jolt.http.server :as http]
            [oscope.ui.events :as events]
            [oscope.ui.visualization-editor :as visualization-editor]
            [oscope.ui.web :as web]
            [oscope.ui.workbench :as workbench]))

(def default-host "127.0.0.1")

(def ^:private text-headers
  {"Content-Type" "text/plain; charset=UTF-8"
   "Cache-Control" "no-store"
   "X-Content-Type-Options" "nosniff"})

(defn- header-value [request name]
  (let [wanted (str/lower-case name)]
    (some (fn [[key value]]
            (when (= wanted (str/lower-case
                             (if (keyword? key) (name key) (str key))))
              (str/trim (str value))))
          (:headers request))))

(defn handler
  "Compose only oscope's bounded viewer handlers behind an exact Host check."
  [{:keys [authority workbench-handler events-handler aggregate-handler
           editor-handler]}]
  (fn [{:keys [request-method uri] :as request}]
    (let [expected (if (fn? authority) (authority) authority)]
      (cond
        (or (nil? expected) (not= expected (header-value request "host")))
        {:status 421 :headers (assoc text-headers "Connection" "close")
         :body "misdirected request\n"}

        (workbench/handled-path? workbench/default-path uri)
        (workbench-handler request)

        (events/handled-path? events/default-path uri)
        (events-handler request)

        (visualization-editor/handled-path?
         visualization-editor/default-path uri)
        (editor-handler request)

        (web/handled-path? web/default-path uri)
        (aggregate-handler request)

        (and (= :get request-method) (= "/healthz" uri))
        {:status 200 :headers text-headers :body "ok\n"}

        (and (= :get request-method) (= "/" uri))
        {:status 303
         :headers {"Location" workbench/default-path
                   "Cache-Control" "no-store"}
         :body ""}

        :else
        {:status 404 :headers text-headers :body "not found\n"}))))

(defn start!
  "Start an oscope viewer for an already-owned embedded lifecycle."
  [{:keys [source connection host port]
    :or {host default-host port 4320}}]
  (when-not (= default-host host)
    (throw (ex-info "embedded viewer must bind to 127.0.0.1"
                    {:voxel.telemetry/error true :type ::unsafe-host
                     :host host})))
  (when-not (and (integer? port) (<= 0 port 65535))
    (throw (ex-info "embedded viewer port must be between 0 and 65535"
                    {:voxel.telemetry/error true :type ::invalid-port
                     :port port})))
  (when-not (and source connection)
    (throw (ex-info "embedded viewer requires an oscope source and connection"
                    {:voxel.telemetry/error true :type ::invalid-source})))
  (let [authority* (atom (when (pos? port) (str host ":" port)))
        editor (visualization-editor/handler source)
        app (handler
             {:authority #(deref authority*)
              :workbench-handler (workbench/handler connection)
              :events-handler (events/handler connection)
              :aggregate-handler
              (web/handler
               source
               {:visualization-editor-path
                (visualization-editor/plotje-path
                 visualization-editor/default-path)})
              :editor-handler editor})
        server (http/run-server app :port port :server-name host
                                :reuse-address? true :pool-size 2)
        stopped? (atom false)]
    (reset! authority* (str host ":" (:port server)))
    {:host host
     :port (:port server)
     :url (str "http://" host ":" (:port server) workbench/default-path)
     :handler app
     :server server
     :stopped? stopped?
     :stop! #(when (compare-and-set! stopped? false true)
               (http/stop-server server))}))

(defn stop! [viewer]
  (when-let [stop-fn (:stop! viewer)] (stop-fn))
  true)
