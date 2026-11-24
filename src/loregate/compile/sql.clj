(ns loregate.compile.sql
  "Typed IR → one DuckDB statement (SPEC 7, step 7).

  Every source is a CTE over the relation src_<alias>, in two levels: the inner one casts
  the needed fields to their declared types and maps enum values (true values); the outer
  one applies the row rule as its WHERE and masks fields (SPEC 8.2). Everything after the
  CTE sees ontology names, types and only what the user may see. Identifiers are quoted.
  Every value is a positional parameter cast to its type; the parameters are returned
  beside the SQL in order, as descriptors:
  {:value v} | {:param k} | {:clock :now|:today} | {:user k} | {:visible rule-id}."
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
    (:string :enum nil) "VARCHAR" :int "BIGINT" :boolean "BOOLEAN"
    :decimal (str "DECIMAL(38," (or scale 6) ")")
    :date "DATE" :datetime "TIMESTAMP"          ; ponytail: UTC only; zones need TIMESTAMPTZ + ICU
    :set (str (sql-type {:type of}) "[]")))

(defn- sorted [xs] (sort-by str xs))

(defn- col [alias attr] (str (q (name alias)) "." (q (kw-name attr))))

(defn compiler
  "A fn compiling typed IR to SQL text, appending parameter descriptors to atom `ps` in
  text order. `params` are the query's param definitions."
  [ps params]
  (let [param! (fn [descriptor type] (swap! ps conj descriptor) (str "CAST(? AS " (sql-type type) ")"))]
    (letfn [(lit-elem [v target]
              (if (qualified-keyword? v)
                (param! {:value v} {:type :string})
                (param! {:value v} {:type (or target (cond (string? v) :string (boolean? v) :boolean
                                                           (integer? v) :int :else :decimal))})))
            (list-e [ir]
              (if (= ::r/lit-set (first ir))
                (str "[" (str/join ", " (mapv #(lit-elem % nil) (sorted (second ir)))) "]")
                (e ir)))
            (e [ir]
              (case (first ir)
                ::r/col   (col (second ir) (nth ir 2))
                ::r/lit   (param! {:value (second ir)} {:type (nth ir 2)})
                ::r/enum  (param! {:value (second ir)} {:type :string})
                ::r/param (param! {:param (second ir)} {:type (get-in params [(second ir) :type])})
                ::r/user  (param! {:user (second ir)} (or (nth ir 2 nil) {:type :string}))
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
                              ;; list_contains(coll, x): coll comes first in the text, so its
                              ;; parameters are added first
                              (let [cs (e coll) xs (e x)]
                                (str "list_contains(" cs ", " xs ")"))))
                :overlaps (let [[_ a b] ir] (str "list_has_any(" (list-e a) ", " (list-e b) ")"))
                :subset   (let [[_ a b] ir
                                ;; every element of a is in b; b comes first in the text
                                bs (list-e b) as (list-e a)]
                            (str "list_has_all(" bs ", " as ")"))
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
                :date-trunc (let [[_ unit x] ir] (str "date_trunc('" (name unit) "', " (e x) ")"))))]
      {:e e :param! param!})))

(defn- field-sql
  "A stored field cast to its declared type (enum values mapped) — the true value."
  [{:keys [param!]} {:keys [binding type-info]} attr]
  (let [f (get-in binding [:binding/fields attr])
        decl (get-in type-info [:type/attributes attr])
        raw (if (string? f)
              (q f)
              ;; DuckDB needs a wildcard path as a constant; the path comes from the binding
              ;; only, never from a request
              (str "json_extract_string(" (q (first f)) ", '" (str/replace (second f) "'" "''") "')"))]
    (if (= :enum (:type decl))
      (str "CASE WHEN " raw " IS NULL THEN NULL"
           (apply str (for [[v stored] (sort-by (comp str key) (get-in binding [:binding/values attr]))]
                        (str " WHEN " raw " = " (param! {:value stored} {:type :string})
                             " THEN " (param! {:value v} {:type :string}))))
           " ELSE error('loregate:schema-mismatch') END")
      (str "CAST(" raw " AS " (sql-type decl) ")"))))

(defn- cte
  "One source: outer level (masks, row rule) over the inner level (true values)."
  [{:keys [e param!] :as c} {:keys [alias needed rule-needed row-rule field-rules] :as s}]
  (let [a (q (name alias))
        outer (str/join ", " (mapv (fn [attr]
                                     (let [{:keys [level ir rule]} (get field-rules attr)
                                           v (col alias attr)]
                                       (str (case level
                                              :user (str "CASE WHEN " (param! {:visible rule} {:type :boolean}) " THEN " v " END")
                                              :row  (str "CASE WHEN " (e ir) " THEN " v " END")
                                              nil v)
                                            " AS " (q (kw-name attr)))))
                                   (sorted needed)))
        inner (str/join ", " (mapv #(str (field-sql c s %) " AS " (q (kw-name %)))
                                   (sorted (into (set needed) rule-needed))))
        where (when row-rule (str " WHERE " (e row-rule)))]
    (str a " AS (SELECT " outer " FROM (SELECT " inner " FROM " (q (str "src_" (name alias))) ") AS " a where ")")))

(defn generate
  "{:sql … :params […]}. Takes :sources (ordered; each :alias :type-info :binding :needed
  :rule-needed :row-rule :field-rules :parent-alias :link), :select [{:name :ir :type}],
  :where, :having, :group (positions), :order [[position dir]], :limit and :params."
  [{:keys [sources select where having group order limit params]}]
  (let [ps (atom [])
        {:keys [e] :as c} (compiler ps params)
        ctes   (mapv #(cte c %) sources)
        cols   (mapv (fn [{:keys [name ir type]}] (str "CAST(" (e ir) " AS " (sql-type type) ") AS " (q (kw-name name)))) select)
        [root & joined] sources
        joins  (mapv (fn [{:keys [alias parent-alias link]}]
                       (str " LEFT JOIN " (q (name alias)) " ON "
                            (str/join " AND " (for [[fa ta] (:link/on link)]
                                                (str (col parent-alias fa) " = " (col alias ta))))))
                     joined)
        where' (when where (str " WHERE " (e where)))
        having' (when having (str " HAVING " (e having)))]
    {:sql (str "WITH " (str/join ", " ctes)
               " SELECT " (str/join ", " cols)
               " FROM " (q (name (:alias root))) (apply str joins)
               where'
               (when (seq group) (str " GROUP BY " (str/join ", " group)))
               having'
               " ORDER BY " (str/join ", " (for [[pos dir] order] (str pos " " (str/upper-case (name dir)) " NULLS LAST")))
               (when limit (str " LIMIT " (long limit))))
     :params @ps}))

(defn checks
  "{:sql \"SELECT CAST(e AS BOOLEAN), …\" :params […] :rules [rule-id …]} evaluating
  user-level field rules once per run (their IR reads no column)."
  [rules]                                    ; [[rule-id ir] …]
  (let [ps (atom [])
        {:keys [e]} (compiler ps {})
        exprs (mapv (fn [[_ ir]] (str "CAST(" (e ir) " AS BOOLEAN)")) rules)]
    {:sql (str "SELECT " (if (seq exprs) (str/join ", " exprs) "TRUE"))
     :params @ps
     :rules (mapv first rules)}))
