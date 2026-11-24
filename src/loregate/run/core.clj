(ns loregate.run.core
  "Execute a plan for a user (SPEC 9): check pins, validate params, fix the clock, attach
  the sources, run DuckDB, shape rows and notes."
  (:require [loregate.compile.core :as compile]
            [loregate.compile.sql :as sql]
            [loregate.ontology.port :as o]
            [loregate.run.duck :as duck]
            [loregate.sources.port :as src])
  (:import (java.time Instant LocalDate LocalDateTime ZoneId ZoneOffset)
           (java.util Date)))

(defn stale
  "Ids whose pin differs from the ontology now, plus :plan/compiler if the compiler
  changed; empty when the plan is current."
  [onto plan]
  (cond-> (vec (sort-by str (for [[id h] (:plan/pins plan) :when (not= h (o/hash-of onto id))] id)))
    (not= compile/version (:plan/compiler plan)) (conj :plan/compiler)))

(defn- coerce-param [{:keys [type]} v]
  (try
    (case type
      :string   (when (string? v) v)
      :int      (when (integer? v) (long v))
      :decimal  (cond (number? v) (bigdec v) (string? v) (bigdec v))
      :boolean  (when (boolean? v) v)
      :date     (cond (instance? LocalDate v) v (string? v) (LocalDate/parse v))
      :datetime (cond (instance? LocalDateTime v) v (string? v) (LocalDateTime/parse v)))
    (catch Exception _ nil)))

(defn check-params
  "{:values {k v}} with defaults applied and every value checked, or {:error :params/invalid}."
  [defs given]
  (let [unknown (remove (set (keys defs)) (keys given))
        values (into {} (for [[k d] defs
                              :let [v (if (contains? given k) (get given k) (:default d))]]
                          [k (when (some? v) (coerce-param d v))]))
        bad (for [[k d] defs
                  :let [v (get values k)]
                  :when (or (nil? v)
                            (and (:min d) (number? v) (< v (:min d)))
                            (and (:max d) (number? v) (> v (:max d)))
                            (and (:options d) (not (some #(= v (coerce-param d %)) (:options d)))))]
              k)]
    (if (or (seq unknown) (seq bad))
      {:error :params/invalid :unknown (vec unknown) :invalid (vec (sort-by str bad))}
      {:values values})))

(defn- jdbc-value [v]
  (cond (keyword? v) (sql/kw-name v)
        :else v))

(defn- supplied-relation
  "Rows given in ontology names → storage columns and values, through the binding."
  [alias {:keys [fields values]} rows]
  (let [attrs (sort-by str (keys fields))]
    (if-let [json (seq (filter #(vector? (fields %)) attrs))]
      ;; ponytail: JSON-path fields cannot be supplied yet; build the JSON column when needed
      {:error :params/invalid :reason :json-field-in-supplied-rows :attributes (vec json)}
      (let [stored (fn [attr v]
                     (if-let [m (get values attr)]
                       (if (nil? v) nil (or (get m v) (throw (ex-info "unmapped" {:attr attr :value v}))))
                       v))]
        (try
          {:alias alias
           :columns (mapv (fn [a] [(fields a) (boolean (some #(coll? (get % a)) rows))]) attrs)
           :rows (mapv (fn [row] (mapv #(stored % (get row %)) attrs)) rows)}
          (catch clojure.lang.ExceptionInfo e
            {:error :source/schema-mismatch :detail (ex-data e)}))))))

(defn- shape-value [{:keys [type]} v]
  (cond (nil? v) nil
        (= :enum type) (keyword v)
        (= :set type) (set v)
        (instance? LocalDateTime v) (Date/from (.toInstant ^LocalDateTime v ZoneOffset/UTC))
        :else v))

(defn run-plan
  "Rows and notes, or {:error …}. `system` holds :ontology, :sources and :config;
  `opts` :params, :user, :relations (rows per source path, library only) and :clock
  ({:now inst :zone \"…\"}) in place of the system clock."
  [{:keys [ontology sources config]} plan {:keys [params relations clock user]}]
  (let [changed (stale ontology plan)
        p (check-params (:plan/params plan) (or params {}))]
    (cond
      (seq changed) {:error :plan/stale :changed changed}
      (:error p) p
      :else
      (let [zone (or (:zone clock) (:zone config))
            now (Instant/ofEpochMilli (.getTime ^Date (or (:now clock) (Date.))))
            today (LocalDate/ofInstant now (ZoneId/of zone))
            attrs (:user/attrs user)
            value-of (fn [visible {:keys [value param clock] :as d}]
                       (cond param (get-in p [:values param])
                             (contains? d :user) (get attrs (:user d))      ; absent → NULL → rule false
                             (contains? d :visible) (get visible (:visible d))
                             (= :now clock) (LocalDateTime/ofInstant now ZoneOffset/UTC)
                             (= :today clock) today
                             :else (jdbc-value value)))
            {checks-sql :sql checks-params :params check-rules :rules} (:plan/checks plan)
            visible (zipmap check-rules
                            (map true? (duck/evaluate checks-sql (mapv #(value-of {} %) checks-params))))
            values (mapv #(value-of visible %) (:plan/sql-params plan))
            rels (vec (for [[alias s] (sort-by key (:plan/sources plan))]
                        (if (contains? relations (:path s))
                          (supplied-relation alias s (vec (get relations (:path s))))
                          (assoc (src/relation (get sources (:kind s)) s) :alias alias))))
            explicit-limit? (contains? (:plan/query plan) :limit)
            max-rows (if explicit-limit? Long/MAX_VALUE (:max-rows config))]
        (if-let [e (some #(when (:error %) %) rels)]
          e
          (let [res (duck/execute {:relations rels :sql (:plan/sql plan) :params values
                                   :max-rows (min max-rows Integer/MAX_VALUE)
                                   :memory-limit (get-in config [:duckdb :memory-limit])
                                   :threads (get-in config [:duckdb :threads])})]
            (if (:error res)
              res
              (let [out (:plan/output plan)
                    truncated? (> (count (:rows res)) max-rows)
                    rows (cond->> (:rows res) truncated? (take max-rows))
                    now-date (Date/from now)]
                {:rows (mapv (fn [r] (into {} (map (fn [c v] [(:name c) (shape-value c v)]) out r))) rows)
                 :columns out
                 :notes (cond-> {:plan (:plan/id plan)
                                 :as-of (into {} (for [alias (keys (:plan/sources plan))] [alias now-date]))
                                 :clock {:now now-date :today (str today) :zone zone}
                                 :hidden (into #{} (comp (filter (comp false? val)) (map (comp second key))) visible)
                                 :partial (into #{} (comp (filter (comp #{:row} val)) (map key)) (get-in plan [:plan/rules :fields]))
                                 :rows-filtered (get-in plan [:plan/rules :rows])}
                          truncated? (assoc :truncated true))}))))))))
