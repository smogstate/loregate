(ns loregate.config
  "The one list of settings (SPEC 4.2): defaults here, a settings EDN file over them, and
  environment variables over both. A variable overrides a setting that exists: its name is
  LOREGATE_ plus the setting's path, upper-cased, non-alphanumerics as _ —
  [:http :port] → LOREGATE_HTTP_PORT, :storage/allowed → LOREGATE_STORAGE_ALLOWED. Values
  are read as EDN (8080, [\"a\" \"b\"], true), anything else as a string."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def defaults
  {:max-limit 10000                 ; largest :limit a query may ask for
   :max-rows  10000                 ; rows returned when a query sets no :limit
   :zone      "UTC"                 ; time zone of [:now] and [:today]
   :duckdb    {:memory-limit "1GB" :threads 4}
   :source-timeout-ms 10000         ; per fetch source
   :run-timeout-ms 30000            ; whole run
   :max-concurrent-runs 4           ; per user; more → :budget/busy
   :publish/role nil                ; role allowed to publish plans; nil → nobody
   :storage/allowed []              ; location prefixes Parquet bindings may read

   ;; adapters (loregate.main builds the system from these)
   :ontology {:adapter :edn :path "ontology.edn"}
   :store    {:adapter :none}       ; :none | :memory | :graph (with :path)
   :sources  {:parquet {}           ; reads under :storage/allowed
              :service nil}         ; {:bases {"service-name" "https://…"}} to enable
   :identity {:adapter :oidc :issuer nil :audience nil :jwks-uri nil
              :claims {:roles "roles" :groups "groups"}}
   :http     {:host "0.0.0.0" :port 8080}})

(defn- deep-merge [a b]
  (if (and (map? a) (map? b)) (merge-with deep-merge a b) b))

(defn- leaf-paths [m prefix]
  (mapcat (fn [[k v]] (if (and (map? v) (seq v) (keyword? k) (not= k :claims))
                        (leaf-paths v (conj prefix k))
                        [(conj prefix k)]))
          m))

(defn env-name [path]
  (str "LOREGATE_" (str/upper-case (str/replace (str/join "_" (map #(if (keyword? %) (subs (str %) 1) (str %)) path))
                                                #"[^A-Za-z0-9_]" "_"))))

(defn- env-value [s]
  (let [v (try (edn/read-string s) (catch Exception _ ::string))]
    (if (or (= ::string v) (symbol? v)) s v)))

(defn resolve-settings
  "Defaults, then `file-settings`, then `env` (a map of variable → string)."
  [file-settings env]
  (let [merged (deep-merge defaults file-settings)]
    (reduce (fn [s path]
              (if-let [v (get env (env-name path))]
                (assoc-in s path (env-value v))
                s))
            merged
            (leaf-paths merged []))))

(defn load-settings
  "Settings from the EDN file at `path` (or none) and the process environment."
  [path]
  (resolve-settings (if path (edn/read-string (slurp path)) {}) (into {} (System/getenv))))
