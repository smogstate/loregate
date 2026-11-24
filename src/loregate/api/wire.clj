(ns loregate.api.wire
  "Bodies over HTTP (SPEC 10): EDN or JSON in, the same value encoding out.

  JSON has no keywords, so a JSON query names things with strings:
  - a string \"ns/name\" is a qualified keyword (an attribute, measure, link, enum value);
  - an array whose first element is an operator name is an expression;
  - any other array of names is a path;
  - a string that names a :let local is that local; any other string is a string;
  - {\"$str\": \"a/b\"} is the string \"a/b\" (escape), {\"$set\": […]} a literal set."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [loregate.compile.sql :refer [kw-name]]
            [loregate.schema.expr :as expr])
  (:import (java.math BigDecimal)
           (java.time LocalDate LocalDateTime ZoneOffset)
           (java.util Date)))

;; ------------------------------------------------------------ JSON → DSL

(def ^:private qualified-re #"[A-Za-z_][\w.\-]*/[\w.\-?!*+<>=]+")
(def ^:private op-names (into #{} (map name) (into (set (keys expr/operators)) [:param :user :date-trunc])))

(defn- qualified-str? [x] (and (string? x) (re-matches qualified-re x)))

(defn- scalar [x]
  (cond (qualified-str? x) (keyword x)
        (and (map? x) (contains? x "$str")) (get x "$str")
        :else x))

(defn ->expr
  "A JSON value in an expression position → DSL."
  [x locals]
  (cond
    (and (map? x) (contains? x "$set")) (into #{} (map scalar) (get x "$set"))
    (and (map? x) (contains? x "$str")) (get x "$str")
    (qualified-str? x) (keyword x)
    (and (string? x) (contains? locals x)) (keyword x)
    (and (vector? x) (string? (first x)) (op-names (first x)))
    (let [[op & args] x]
      (case op
        ("param" "user") [(keyword op) (keyword (first args))]
        "date-trunc" [:date-trunc (keyword (first args)) (->expr (second args) locals)]
        (into [(keyword op)] (map #(->expr % locals)) args)))
    (and (vector? x) (seq x) (every? qualified-str? x)) (mapv keyword x)
    :else x))

(defn- param-def [d]
  (cond-> (into {} (map (fn [[k v]] [(keyword k) v])) d)
    (get d "type") (update :type keyword)
    (get d "default") (update :default scalar)
    (get d "options") (update :options #(mapv scalar %))))

(defn ->query
  "A JSON query object → the DSL map."
  [q]
  (let [locals (set (keys (get q "let")))
        e #(->expr % locals)]
    (cond-> {}
      (contains? q "from")   (assoc :from (keyword (get q "from")))
      (contains? q "select") (assoc :select (mapv e (get q "select")))
      (contains? q "let")    (assoc :let (into {} (for [[k v] (get q "let")] [(keyword k) (e v)])))
      (contains? q "where")  (assoc :where (e (get q "where")))
      (contains? q "having") (assoc :having (e (get q "having")))
      (contains? q "sort")   (assoc :sort (mapv (fn [[r d]] [(e r) (keyword d)]) (get q "sort")))
      (contains? q "limit")  (assoc :limit (get q "limit"))
      (contains? q "params") (assoc :params (into {} (for [[k d] (get q "params")] [(keyword k) (param-def d)]))))))

(defn- ->params [p] (into {} (for [[k v] p] [(keyword k) v])))

(defn read-body
  "Parse a request body: {:ok value} or {:error :request/invalid}. `kind` :query (the
  body is a query) or :request (a map with :query and/or :params)."
  [^String body json? kind]
  (try
    (if json?
      (let [v (json/read-str (or body "") :bigdec true)]
        {:ok (case kind
               :query (->query v)
               :request (cond-> {}
                          (get v "query") (assoc :query (->query (get v "query")))
                          (get v "params") (assoc :params (->params (get v "params"))))
               :publish {:name (get v "name") :description (get v "description")})})
      (let [v (edn/read-string (or body ""))]
        {:ok (case kind
               :request (select-keys v [:query :params])
               :publish (select-keys v [:name :description])
               v)}))
    (catch Exception _ {:error :request/invalid})))

;; ------------------------------------------------------------ out

(defn- plain [v]
  (cond (instance? BigDecimal v) (.toPlainString ^BigDecimal v)
        (instance? LocalDate v) (str v)
        (instance? Date v) (str (.toInstant ^Date v))
        (instance? LocalDateTime v) (str (.toInstant ^LocalDateTime v ZoneOffset/UTC))
        :else v))

(defn edn-str [x] (pr-str (walk/postwalk plain x)))

(defn json-str [x]
  (json/write-str (walk/postwalk #(cond (keyword? %) (kw-name %)
                                        (set? %) (vec (sort-by str %))
                                        :else (plain %))
                                 x)
                  :key-fn #(if (string? %) % (pr-str %))))
