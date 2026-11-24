(ns loregate.main
  "Build a system from settings and serve it over HTTP:
    java -jar loregate.jar settings.edn      (or: clojure -M:serve settings.edn)"
  (:require [loregate.api.http :as http]
            [loregate.auth.oidc :as oidc]
            [loregate.config :as config]
            [loregate.core :as lg]
            [loregate.ontology.memory :as mem]
            [loregate.sources.parquet :as pq]
            [loregate.sources.port :as src]
            [loregate.sources.service :as svc]
            [loregate.store.graph :as graph]
            [loregate.store.memory :as memory]
            [loregate.store.none :as none]))

(defn sources [settings]
  (cond-> {}
    (get-in settings [:sources :parquet]) (assoc :parquet (pq/adapter (:storage/allowed settings)))
    (get-in settings [:sources :service]) (assoc :service (svc/adapter (svc/http-transport
                                                                        (get-in settings [:sources :service :bases])
                                                                        :timeout-ms (:source-timeout-ms settings))))))

(defn- store [{:keys [adapter path]}]
  (case adapter
    :none (none/store)
    :memory (memory/store)
    :graph (graph/store path)))

(defn- ontology [{:keys [adapter path]} srcs]
  (case adapter
    :edn (mem/create (mem/load-edn path)
                     {:schema-hash (fn [b] (some-> (get srcs (:binding/kind b)) (src/schema-hash b)))})))

(defn identity-of [{:keys [adapter] :as cfg}]
  (case adapter
    :oidc (oidc/adapter (dissoc cfg :adapter))))

(defn system
  "A loregate.core system from resolved settings."
  [settings]
  (let [srcs (sources settings)]
    (lg/system {:ontology (ontology (:ontology settings) srcs)
                :sources srcs
                :store (store (:store settings))
                :config (dissoc settings :ontology :store :sources :identity :http)})))

(defn handler
  "The HTTP handler for resolved settings."
  [settings]
  (http/handler (system settings) (identity-of (:identity settings))))

(defn -main [& [settings-path]]
  (let [settings (config/load-settings settings-path)
        server (http/start (handler settings) (:http settings))]
    (println "loregate listening on port" (.getPort (.getAddress server)))
    @(promise)))
