(ns voxel.telemetry.config
  "Validated environment configuration for the opt-in telemetry launcher."
  (:require [clojure.string :as str]))

(def default-options
  {:host "127.0.0.1"
   :port 4320
   :storage-root "./naval-telemetry"
   :object-id "naval-battle"
   :owner "naval-battle"
   :database "default"
   :hud-interval-ms 1000
   :checkpoint-on-close? true
   :sdk-options {:service-name "naval-battle"
                 ;; Keep Durable insertion off gameplay threads while making
                 ;; the adjacent viewer useful within one HUD refresh.
                 :processor :batch
                 :schedule-delay-ms 1000
                 :metric-interval-ms 1000
                 :metrics? true
                 :runtime-metrics? true
                 :logs? true
                 :bridge-logging? false}})

(defn- fail! [type message value]
  (throw (ex-info message
                  {:voxel.telemetry/error true :type type :value value})))

(defn- nonblank! [key value]
  (when-not (and (string? value) (not (str/blank? value)))
    (fail! ::invalid-option (str (name key) " must be non-blank") value))
  value)

(defn- port! [value]
  (when-not (and (integer? value) (<= 0 value 65535))
    (fail! ::invalid-port "viewer port must be between 0 and 65535" value))
  value)

(defn normalize
  "Validate a complete launcher option map before storage or listeners exist."
  [options]
  (when-not (map? options)
    (fail! ::invalid-options "telemetry options must be a map" options))
  (let [options (merge default-options options)
        host (nonblank! :host (:host options))
        sdk-options (merge (:sdk-options default-options)
                           (:sdk-options options))]
    (when-not (= "127.0.0.1" host)
      (fail! ::unsafe-host "the embedded viewer must bind to 127.0.0.1" host))
    (when-not (map? (:sdk-options options))
      (fail! ::invalid-sdk-options "sdk-options must be a map"
             (:sdk-options options)))
    (when-not (boolean? (:checkpoint-on-close? options))
      (fail! ::invalid-option "checkpoint-on-close? must be boolean"
             (:checkpoint-on-close? options)))
    (when-not (and (integer? (:hud-interval-ms options))
                   (<= 250 (:hud-interval-ms options) 60000))
      (fail! ::invalid-option
             "hud-interval-ms must be between 250 and 60000"
             (:hud-interval-ms options)))
    (-> options
        (assoc :host host
               :port (port! (:port options))
               :storage-root (nonblank! :storage-root (:storage-root options))
               :object-id (nonblank! :object-id (:object-id options))
               :owner (nonblank! :owner (:owner options))
               :database (nonblank! :database (:database options))
               :sdk-options sdk-options))))

(defn- parse-port [value]
  (if (str/blank? value)
    (:port default-options)
    (try
      (parse-long value)
      (catch Throwable _
        (fail! ::invalid-port "VOXEL_OTEL_VIEWER_PORT must be an integer" value)))))

(defn env-options
  "Read the small, non-secret launcher surface from an environment-shaped map."
  ([] (env-options #(System/getenv %)))
  ([getenv]
   (normalize
    {:host (or (not-empty (getenv "VOXEL_OTEL_VIEWER_HOST"))
               (:host default-options))
     :port (parse-port (getenv "VOXEL_OTEL_VIEWER_PORT"))
     :storage-root (or (not-empty (getenv "VOXEL_OTEL_STORAGE_ROOT"))
                       (:storage-root default-options))
     :object-id (or (not-empty (getenv "VOXEL_OTEL_OBJECT_ID"))
                    (:object-id default-options))
     :owner (or (not-empty (getenv "VOXEL_OTEL_OWNER"))
                (:owner default-options))
     :database (or (not-empty (getenv "VOXEL_OTEL_DATABASE"))
                   (:database default-options))
     ;; Keep identity aligned with the external-collector acceptance phase.
     :sdk-options (assoc (:sdk-options default-options)
                         :service-name
                         (or (not-empty (getenv "OTEL_SERVICE_NAME"))
                             (get-in default-options
                                     [:sdk-options :service-name])))})))
