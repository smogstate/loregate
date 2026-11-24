(ns loregate.compile.sql
  "Typed IR → one DuckDB statement (SPEC 7, step 7).

  Every source is a CTE over the relation src_<alias>: it selects the needed fields, casts
  each to its declared type and maps enum values, so everything after it sees ontology
  names and types only. Identifiers are quoted. Every value is a positional parameter,
  cast to its type; the parameters are returned beside the SQL in order, as descriptors:
  {:value v} | {:param k} | {:clock :now|:today}."
  (:require [clojure.string :as str]
            [loregate.compile.resolve :as r]))

(defn q
  "A quoted identifier."
  [s]
  (str "\"" (str/replace s "\"" "\"\"") "\""))

(defn kw-name
  "Column name for a keyword: \"biz/amount\", \"n\"."
  [k]
  (subs (str k) 1))

(defn sql-type [{:keys [type of scale]}]
  (case type
    :string "VARCHAR" :enum "VARCHAR" :int "BIGINT" :boolean "BOOLEAN"
    :decimal (str "DECIMAL(38," (or scale 6) ")")
    :date "DATE" :datetime "TIMESTAMP"          ; ponytail: UTC only; zones need TIMESTAMPTZ + ICU
    :set (str (sql-type {:type of}) "[]")))

(defn- sorted [xs] (sort-by str xs))

(defn generate
  "{:sql … :params […]}. `plan` holds :sources (ordered, each with :alias :type-info
  :binding :needed :parent-alias :link), :select [{:name :ir :type}], :where, :having,
  :group (positions), :order [[position dir]], :limit and :params (the query's)."
  [{:keys [sources select where having group order limit params]}]
  (let [ps (atom [])
        param! (fn [descriptor type] (swap! ps conj descriptor) (str "CAST(? AS " (sql-type type) ")"))
        col (fn [alias attr] (str (q (name alias)) "." (q (kw-name attr))))]
    (letfn [(e [ir]
              (case (first ir)
                ::r/col   (col (second ir) (nth ir 2))
                ::r/lit   (param! {:value (second ir)} {:type (nth ir 2)})
                ::r/enum  (param! {:value (second ir)} {:type :string})
                ::r/param (param! {:param (second ir)} {:type (get-in params [(second ir) :type])})
                ::r/def   (e (nth ir 2))
                ::r/agg   (let [[_ op filter & args] ir
                                inner (case op
                                        :count (if (seq args) (str "count(" (e (first args)) ")") "count(*)")
                                        :count-distinct (str "count(DISTINCT " (e (first args)) ")")
                                        (str (name op) "(" (e (first args)) ")"))]
                            (if filter (str inner " FILTER (WHERE " (e filter) ")") inner))
                (:= :not= :< :<= :> :>=)
                (let [[_ a b] ir]
                  (str "(" (e a) " " ({:= "=" :not= "<>" :< "<" :<= "<=" :> ">" :>= ">="} (first ir)) " " (e b) ")"))
                :between  (let [[_ x lo hi] ir] (str "(" (e x) " BETWEEN " (e lo) " AND " (e hi) ")"))
                :is-null  (str "(" (e (second ir)) " IS NULL)")
                :in       (let [[_ x coll] ir]
                            (if (= ::r/lit-set (first coll))
                              (let [xs (e x)]
                                (str "(" xs " IN (" (str/join ", " (mapv #(lit-elem % (nth coll 2 nil)) (sorted (second coll)))) "))"))
                              (let [xs (e x)] (str "list_contains(" (e coll) ", " xs ")"))))
                :overlaps (let [[_ a b] ir] (str "list_has_any(" (list-e a) ", " (list-e b) ")"))
                :subset   (let [[_ a b] ir]
                            ;; every element of a is in b; b comes first in the text, so its
                            ;; parameters must be added first
                            (let [bs (list-e b) as (list-e a)] (str "list_has_all(" bs ", " as ")")))
                :and      (str "(" (str/join " AND " (mapv e (rest ir))) ")")
                :or       (str "(" (str/join " OR " (mapv e (rest ir))) ")")
                :not      (str "(NOT " (e (second ir)) ")")
                (:+ :- :*) (str "(" (str/join (str " " (name (first ir)) " ") (mapv e (rest ir))) ")")
                :/        (str "(" (str/join " / " (mapv #(str "CAST(" (e %) " AS DOUBLE)") (rest ir))) ")")
                :if       (let [[_ c a b] ir] (str "(CASE WHEN " (e c) " THEN " (e a) " ELSE " (e b) " END)"))
                :coalesce (str "coalesce(" (str/join ", " (mapv e (rest ir))) ")")
                :lower    (str "lower(" (e (second ir)) ")")
                :contains (let [[_ a b] ir] (str "contains(" (e a) ", " (e b) ")"))
                :starts-with (let [[_ a b] ir] (str "starts_with(" (e a) ", " (e b) ")"))
                :now      (param! {:clock :now} {:type :datetime})
                :today    (param! {:clock :today} {:type :date})
                :days-between (let [[_ a b] ir]
                                (str "date_diff('day', CAST(" (e a) " AS DATE), CAST(" (e b) " AS DATE))"))
                :date-trunc (let [[_ unit x] ir] (str "date_trunc('" (name unit) "', " (e x) ")"))))
            (lit-elem [v target]
              (if (qualified-keyword? v)
                (param! {:value v} {:type :string})
                (param! {:value v} {:type (or target (cond (string? v) :string (boolean? v) :boolean
                                                           (integer? v) :int :else :decimal))})))
            (list-e [ir]
              (if (= ::r/lit-set (first ir))
                (str "[" (str/join ", " (mapv #(lit-elem % nil) (sorted (second ir)))) "]")
                (e ir)))
            (field [{:keys [binding type-info]} attr]
              (let [f (get-in binding [:binding/fields attr])
                    decl (get-in type-info [:type/attributes attr])
                    raw (if (string? f)
                          (q f)
                          ;; DuckDB needs a wildcard path as a constant; the path comes from the
                          ;; binding only, never from a request
                          (str "json_extract_string(" (q (first f)) ", '" (str/replace (second f) "'" "''") "')"))]
                (if (= :enum (:type decl))
                  (str "CASE WHEN " raw " IS NULL THEN NULL"
                       (apply str (for [[v stored] (sort-by (comp str key) (get-in binding [:binding/values attr]))]
                                    (str " WHEN " raw " = " (param! {:value stored} {:type :string})
                                         " THEN " (param! {:value v} {:type :string}))))
                       " ELSE error('loregate:schema-mismatch') END")
                  (str "CAST(" raw " AS " (sql-type decl) ")"))))
            (cte [{:keys [alias needed] :as s}]
              (str (q (name alias)) " AS (SELECT "
                   (str/join ", " (mapv #(str (field s %) " AS " (q (kw-name %))) (sorted needed)))
                   " FROM " (q (str "src_" (name alias))) ")"))]
      (let [ctes   (mapv cte sources)
            cols   (mapv (fn [{:keys [name ir type]}] (str "CAST(" (e ir) " AS " (sql-type type) ") AS " (q (kw-name name)))) select)
            [root & joined] sources
            joins  (mapv (fn [{:keys [alias parent-alias link]}]
                           (str " LEFT JOIN " (q (name alias)) " ON "
                                (str/join " AND " (for [[fa ta] (:link/on link)]
                                                    (str (col parent-alias fa) " = " (col alias ta))))))
                         joined)
            where' (when where (str " WHERE " (e where)))
            having' (when having (str " HAVING " (e having)))
            sql (str "WITH " (str/join ", " ctes)
                     " SELECT " (str/join ", " cols)
                     " FROM " (q (name (:alias root))) (apply str joins)
                     where'
                     (when (seq group) (str " GROUP BY " (str/join ", " group)))
                     having'
                     " ORDER BY " (str/join ", " (for [[pos dir] order] (str pos " " (str/upper-case (name dir)) " NULLS LAST")))
                     (when limit (str " LIMIT " (long limit))))]
        {:sql sql :params @ps}))))
