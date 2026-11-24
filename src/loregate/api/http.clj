(ns loregate.api.http
  "HTTP routes (SPEC 10) as one handler: request map → response map, so any host (the
  JDK server below, a Lambda adapter) can carry it. Every route authenticates first; the
  user comes from the token only."
  (:require [clojure.string :as str]
            [loregate.api.wire :as wire]
            [loregate.auth.port :as auth]
            [loregate.core :as lg])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress URLDecoder)
           (java.nio.charset StandardCharsets)
           (java.util.concurrent Executors)))

(def ^:private status-of
  {:auth/missing-token 401 :auth/invalid-token 401 :auth/groups-truncated 401
   :request/invalid 400 :params/invalid 400
   :plan/not-found 404 :plan/stale 409
   :source/timeout 504 :source/unavailable 502 :source/schema-mismatch 502
   :source/row-cap-exceeded 422
   :budget/busy 429 :store/not-supported 501 :auth/forbidden 403 :internal 500})

(defn- status [res]
  (cond (:errors res) 400
        (:error res) (get status-of (:error res) 500)
        :else 200))

(defn- query-params [qs]
  (into {} (for [kv (str/split (or qs "") #"&") :when (seq kv)
                 :let [[k v] (str/split kv #"=" 2)]]
             [(keyword (URLDecoder/decode k StandardCharsets/UTF_8))
              (URLDecoder/decode (or v "") StandardCharsets/UTF_8)])))

(defn- json? [headers k] (str/includes? (str (get headers k)) "json"))

(defn- respond [req res]
  (let [js (or (json? (:headers req) "accept")
               (and (not (str/includes? (str (get-in req [:headers "accept"])) "edn"))
                    (json? (:headers req) "content-type")))]
    {:status (status res)
     :headers {"content-type" (if js "application/json" "application/edn")}
     :body (if js (wire/json-str res) (wire/edn-str res))}))

(defn- route [sys {:keys [method path headers body query-string]} user]
  (let [js (json? headers "content-type")
        read (fn [kind f] (let [{:keys [ok error]} (wire/read-body body js kind)]
                            (if error {:error error} (f ok))))
        [_ id action] (re-matches #"/api/v1/plans/([^/]+)(?:/(run|name))?" path)]
    (cond
      (and (= method "GET") (= path "/api/v1/ontology"))
      (let [{:keys [types search]} (query-params query-string)]
        (lg/describe sys {:user user :search (not-empty search)
                          :types (when (seq types) (mapv keyword (str/split types #",")))}))

      (and (= method "POST") (= path "/api/v1/plans"))
      (read :query #(lg/compile sys %))

      (and (= method "POST") (= path "/api/v1/query"))
      (read :request #(lg/query sys (assoc % :user user)))

      (and id (= method "POST") (= action "run"))
      (read :request #(lg/run sys (assoc % :plan-id id :user user)))

      (and id (= method "GET") (nil? action))
      (lg/plan-status sys {:plan-id id})

      (and id (= method "PUT") (= action "name"))
      (read :publish #(lg/publish sys (assoc % :plan-id id :user user)))

      (and id (= method "DELETE") (= action "name"))
      (lg/unpublish sys {:plan-id id :user user})

      (and (= method "POST") (re-matches #"/api/v1/named/([A-Za-z0-9_-]+)/run" path))
      (let [n (second (re-matches #"/api/v1/named/([A-Za-z0-9_-]+)/run" path))]
        (read :request #(lg/run-named sys (assoc % :name n :user user))))

      :else {:error :request/invalid :reason :no-such-route})))

(defn handler
  "The HTTP handler. `req`: {:method \"GET\" :path \"/api/v1/…\" :query-string …
  :headers {lower-case-name value} :body string}."
  [sys identity]
  (fn [req]
    (respond req
             (try
               (let [user (auth/authenticate identity req)]
                 (if (:error user)
                   user
                   (route sys req user)))
               (catch Exception _
                 ;; never leak SQL, paths or data in an error (SPEC 8.4)
                 {:error :internal})))))

;; ------------------------------------------------------------ JDK server

(defn- exchange->req [^HttpExchange ex]
  {:method (.getRequestMethod ex)
   :path (.getPath (.getRequestURI ex))
   :query-string (.getRawQuery (.getRequestURI ex))
   :headers (into {} (for [[k vs] (.getRequestHeaders ex)] [(str/lower-case k) (first vs)]))
   :body (String. (.readAllBytes (.getRequestBody ex)) StandardCharsets/UTF_8)})

(defn start
  "Serve `handler` on `port` (0 = any free port) with virtual threads. Returns the server;
  (.getPort (.getAddress server)) is the port, (.stop server 0) stops it."
  [handler {:keys [port host] :or {port 8080 host "0.0.0.0"}}]
  (doto (HttpServer/create (InetSocketAddress. ^String host (int port)) 0)
    (.createContext "/" (reify HttpHandler
                          (handle [_ ex]
                            (let [{:keys [status headers body]} (handler (exchange->req ex))
                                  bytes (.getBytes ^String body StandardCharsets/UTF_8)]
                              (doseq [[k v] headers] (.add (.getResponseHeaders ex) k v))
                              (.sendResponseHeaders ex status (alength bytes))
                              (with-open [os (.getResponseBody ex)] (.write os bytes))))))
    (.setExecutor (Executors/newVirtualThreadPerTaskExecutor))
    (.start)))
