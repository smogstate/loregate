(ns loregate.sources.service
  "Service source adapter (SPEC 12.1): a fetch source. Loregate calls a named query of a
  CQRS query service as the user (the caller's token is passed through), checks the rows
  against the query's output schema and loads them as src_<alias>.

  The transport is behind `Transport`. `http-transport` is the shipped one:
    GET  <base>/queries/<query>/schema → the row's JSON Schema
    POST <base>/queries/<query>        {\"args\": {…}, \"cursor\": c|null}
                                       → {\"rows\": [{…}], \"next\": c|null}
  with `Authorization: Bearer <caller's token>`."
  (:require [clojure.data.json :as json]
            [loregate.hash :as h]
            [loregate.sources.port :as port])
  (:import (java.net URI)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.time Duration)))

(defprotocol Transport
  (output-schema [t service query] "The JSON Schema of one row, or nil.")
  (call [t service query args credential cursor] "{:rows […] :next cursor-or-nil} or {:error kind}."))

;; ------------------------------------------------------------ row checks

(defn- json-type-ok? [t v]
  (case t
    "string" (string? v) "integer" (integer? v) "number" (number? v)
    "boolean" (boolean? v) "array" (sequential? v) "object" (map? v)
    "null" (nil? v)
    true))

(defn row-ok?
  "Does `row` (string keys) fit the JSON Schema subset Loregate checks: required properties
  and property types (a type may be a list, e.g. [\"string\" \"null\"])?"
  [schema row]
  (and (map? row)
       (every? #(contains? row %) (get schema "required"))
       (every? (fn [[k {t "type"}]]
                 (or (nil? t) (not (contains? row k))
                     (some #(json-type-ok? % (get row k)) (if (sequential? t) t [t]))))
               (get schema "properties"))))

;; ------------------------------------------------------------ adapter

(defn- columns [binding]
  (set (map #(if (vector? %) (first %) %) (vals (:binding/fields binding)))))

(defrecord Service [transport]
  port/Source
  (kind [_] :service)

  (check [_ binding]
    (let [{:service/keys [name query]} binding
          schema (when (and (string? name) (string? query)) (output-schema transport name query))
          props (set (keys (get schema "properties")))]
      (seq (cond-> []
             (not (and (string? name) (string? query))) (conj {:problem :missing-service-or-query})
             (and name query (nil? schema)) (conj {:problem :no-output-schema})
             schema (into (for [c (sort (columns binding)) :when (not (props c))]
                            {:problem :field-not-in-output-schema :column c}))))))

  (schema-hash [_ binding]
    (h/sha256 (output-schema transport (:service/name binding) (:service/query binding))))

  (relation [_ _] {:fetch true})

  (fetch [_ source args user]
    (let [{:service/keys [name query]} source
          schema (output-schema transport name query)
          cap (or (:row-cap source) Long/MAX_VALUE)]
      (loop [rows [] cursor nil]
        (let [res (call transport name query args (:user/credential user) cursor)]
          (cond
            (:error res) res
            (not (every? #(row-ok? schema %) (:rows res))) {:error :source/schema-mismatch}
            :else
            (let [rows (into rows (:rows res))]
              (cond
                (> (count rows) cap) {:error :source/row-cap-exceeded}   ; never truncate: totals would lie
                (:next res) (recur rows (:next res))
                :else {:rows rows :as-of (java.util.Date.)}))))))))

(defn adapter [transport] (->Service transport))

;; ------------------------------------------------------------ HTTP transport

(defn http-transport
  "The shipped transport. `bases`: {service-name base-url}."
  [bases & {:keys [timeout-ms] :or {timeout-ms 10000}}]
  (let [client (HttpClient/newHttpClient)
        send (fn [^HttpRequest req]
               (try (let [resp (.send client req (HttpResponse$BodyHandlers/ofString))]
                      (if (= 200 (.statusCode resp)) {:ok (json/read-str (.body resp) :bigdec true)} {:error :source/unavailable}))
                    (catch java.net.http.HttpTimeoutException _ {:error :source/timeout})
                    (catch Exception _ {:error :source/unavailable})))
        req (fn [url] (-> (HttpRequest/newBuilder (URI/create url)) (.timeout (Duration/ofMillis timeout-ms))))
        schemas (atom {})]
    (reify Transport
      (output-schema [_ service query]
        (or (get @schemas [service query])
            (when-let [base (get bases service)]
              (let [{:keys [ok]} (send (.build (req (str base "/queries/" query "/schema"))))]
                (when ok (swap! schemas assoc [service query] ok))
                ok))))
      (call [_ service query args credential cursor]
        (if-let [base (get bases service)]
          (let [{:keys [ok error]} (send (-> (req (str base "/queries/" query))
                                             (.header "content-type" "application/json")
                                             (.header "authorization" (str "Bearer " credential))
                                             (.POST (HttpRequest$BodyPublishers/ofString
                                                     (json/write-str {"args" args "cursor" cursor})))
                                             (.build)))]
            (or (when error {:error error})
                {:rows (get ok "rows") :next (get ok "next")}))
          {:error :source/unavailable})))))
