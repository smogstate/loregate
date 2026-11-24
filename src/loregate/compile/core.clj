(ns loregate.compile.core
  "Query → plan (SPEC 7). Pure and deterministic: the same query against the same
  declarations yields the same plan id. Errors are data: {:errors [{:error … :at …}]}."
  (:require [malli.core :as m]
            [malli.error :as me]
            [loregate.compile.binding :as b]
            [loregate.compile.resolve :as r]
            [loregate.compile.sql :as sql]
            [loregate.compile.typecheck :as tc]
            [loregate.hash :as h]
            [loregate.ontology.port :as o]
            [loregate.schema.expr :as expr]
            [loregate.schema.query :as query]
            [loregate.sources.port :as src]))

(def version
  "Bump whenever a change alters generated plans: older plans then read as stale (SPEC 9)."
  1)

(defn- item-name [x]
  (if (= ::expr/path (expr/kind x)) (last x) x))

(defn- nodes [ir] (filter vector? (tree-seq vector? seq ir)))
(defn- has? [pred ir] (some pred (nodes ir)))

(defn- ungrouped-cols
  "Column nodes of `ir` outside any aggregate."
  [ir]
  (cond (not (vector? ir)) nil
        (= ::r/agg (first ir)) nil
        (= ::r/col (first ir)) [ir]
        :else (mapcat ungrouped-cols (rest ir))))

(defn- column [{:keys [name type owned?]}]
  (cond-> {:name name :type (:type type) :owned? owned?}
    (= :set (:type type)) (assoc :of (:of type))
    (:unit type) (assoc :unit (:unit type))))

(defn- trivially-true? [rule-expr] (true? rule-expr))

(defn- rule-ir
  "Resolve and type a rule expression in the scope of one source: stored attributes of
  its type and [:user k] only. Nil when it is literally `true`."
  [ctx env path type at expr]
  (when-not (trivially-true? expr)
    (let [ir (r/resolve-expr ctx {:path path :type type :stack #{} :rule? true} at expr)
          [ir' t] (tc/check ctx env at ir)]
      (when-not (#{:boolean ::tc/error :user} (:type t))
        (r/err! ctx at :compile/type-mismatch :reason :rule-not-boolean))
      ir')))

(defn- source-rules
  "Row rule (layer-enforced sources only) and field rules of the attributes a source
  provides (SPEC 8.2). Fails closed: a layer-enforced type with no row rule is refused."
  [ctx env path ti bnd needed]
  (let [type (:type/id ti)
        rules (:type/rules ti)
        row (some #(when (and (= :rows (:rule/kind %)) (= type (:rule/on %))) %) rules)
        layer? (= :layer (:binding/rows-enforced-by bnd))
        fields (for [attr needed
                     :let [fr (some #(when (and (= :field (:rule/kind %)) (= attr (:rule/on %))) %) rules)]
                     :when fr]
                 [attr fr])]
    (when (and layer? (nil? row))
      (r/err! ctx [:from] :compile/no-row-rule :type type))
    {:row-rule (when (and layer? row) (rule-ir ctx env path type [:rule :rows type] (:rule/allow row)))
     :rule-ids (concat (when (and layer? row) [[:rows type]]) (map (fn [[a _]] [:field a]) fields))
     :field-rules (into {} (for [[attr fr] fields
                                 :let [ir (rule-ir ctx env path type [:rule :field attr] (:rule/visible fr))]
                                 :when ir]
                             [attr {:rule [:field attr] :ir ir
                                    :level (if (some #(= ::r/col (first %)) (nodes ir)) :row :user)}]))}))

(defn field-check
  "How the field rule of `attr` applies on `type`, outside any query: nil (no rule, or
  literally true: visible), {:level :row}, or {:level :user :checks …} to evaluate per user.
  Used by describe (SPEC 10)."
  [onto type attr]
  (let [ctx (r/new-ctx onto nil)
        alias (r/source! ctx [] type nil)
        ti (r/type-info ctx type)
        fr (some #(when (and (= :field (:rule/kind %)) (= attr (:rule/on %))) %) (:type/rules ti))]
    (when fr
      (let [ir (rule-ir ctx {:params {} :alias-types {alias type}} [] type [:rule :field attr] (:rule/visible fr))]
        (cond (seq (:errors @ctx)) {:level :user :checks (sql/checks [])} ; ponytail: a broken rule reads as hidden
              (nil? ir) nil
              (some #(= ::r/col (first %)) (nodes ir)) {:level :row}
              :else {:level :user :checks (sql/checks [[[:field attr] ir]])})))))

(defn compile-query
  "A plan, or {:errors […]}. Options: :sources {kind adapter} (registered source adapters),
  :max-limit (default 10000), :now (for :plan/compiled-at)."
  [onto q {:keys [sources max-limit now] :or {max-limit 10000}}]
  (if-let [e (m/explain query/Query q)]
    {:errors [{:error :compile/invalid-query :at [] :details (me/humanize e)}]}
    (let [ctx (r/new-ctx onto q)
          root (:from q)]
      (if-not (o/type-of onto root)
        {:errors [{:error :compile/unknown-name :at [:from] :name root}]}
        (let [_ (when (and (:limit q) (> (:limit q) max-limit))
                  (r/err! ctx [:limit] :compile/limit-too-large :max max-limit))
              _ (r/source! ctx [] root nil)
              scope {:path [] :type root :stack #{} :filter nil :measures? true :locals (:let q)}
              items (vec (map-indexed
                          (fn [i x] {:name (item-name x) :owned? (not= ::expr/local (expr/kind x))
                                     :item x :at [:select i]
                                     :ir (r/resolve-expr ctx scope [:select i] x)})
                          (:select q)))
              where (some->> (:where q) (r/resolve-expr ctx scope [:where]))
              having (some->> (:having q) (r/resolve-expr ctx scope [:having]))
              alias-types (into {} (map (juxt :alias :type)) (vals (:sources @ctx)))
              env {:params (:params q) :alias-types alias-types}
              typed (fn [ir at] (tc/check ctx env at ir))
              items (mapv (fn [{:keys [ir at] :as it}] (let [[ir' t] (typed ir at)] (assoc it :ir ir' :type t))) items)
              [where wt] (some-> where (typed [:where]))
              [having ht] (some-> having (typed [:having]))
              grouped? (or (some (comp :agg? :type) items) (some? having))
              group (when grouped? (vec (keep-indexed (fn [i it] (when-not (:agg? (:type it)) (inc i))) items)))]
          (doseq [[n its] (group-by :name items) :when (> (count its) 1)]
            (r/err! ctx (:at (second its)) :compile/duplicate-column :name n))
          (when where
            (cond (:agg? wt) (r/err! ctx [:where] :compile/aggregate-in-where)
                  (not (#{:boolean ::tc/error} (:type wt))) (r/err! ctx [:where] :compile/type-mismatch :found (:type wt))))
          (when having
            (when-not (#{:boolean ::tc/error} (:type ht)) (r/err! ctx [:having] :compile/type-mismatch :found (:type ht)))
            (let [keys (map #(:ir (nth items (dec %))) group)]
              (doseq [c (ungrouped-cols having)
                      :when (not-any? #(has? #{c} %) keys)]
                (r/err! ctx [:having] :compile/type-mismatch :reason :not-grouped :column (nth c 2)))))
          (let [sort-pos (vec (keep-indexed
                               (fn [i [x dir]]
                                 (if-let [pos (some (fn [[j it]] (when (= x (:item it)) (inc j))) (map-indexed vector items))]
                                   [pos dir]
                                   (do (r/err! ctx [:sort i] :compile/sort-not-selected :name x) nil)))
                               (:sort q)))
                order (into sort-pos (for [i (range 1 (inc (count items)))
                                           :when (not-any? #(= i (first %)) sort-pos)]
                                       [i :asc]))
                kinds (set (keys sources))
                filters (group-by :alias (b/filters where))
                srcs (vec (for [path (:order @ctx)
                                :let [{:keys [alias type needed parent link]} (get-in @ctx [:sources path])
                                      ti (r/type-info ctx type)
                                      bnd (b/choose (:type/bindings ti) needed (get filters alias) kinds)]]
                            (cond
                              (nil? bnd)
                              (r/err! ctx [:from] :compile/no-binding :type type :path path)
                              (seq (src/check (get sources (:binding/kind bnd)) bnd))
                              (r/err! ctx [:from] :compile/invalid-binding :binding (:binding/id bnd)
                                      :details (src/check (get sources (:binding/kind bnd)) bnd))
                              :else
                              (merge {:alias alias :type type :type-info ti :binding bnd :needed needed :link link :path path
                                      :parent-alias (when parent (get-in @ctx [:sources parent :alias]))}
                                     (source-rules ctx env path ti bnd needed)))))]
            (doseq [{:keys [path binding]} (filter map? srcs)
                    a (get-in @ctx [:sources path :rule-needed])
                    :when (not (contains? (:binding/fields binding) a))]
              (r/err! ctx [:from] :compile/no-binding :binding (:binding/id binding) :reason :rule-attribute-not-bound :attribute a))
            (if-let [errors (seq (:errors @ctx))]
              {:errors (vec errors)}
              (let [srcs (mapv #(assoc % :rule-needed (get-in @ctx [:sources (:path %) :rule-needed])) srcs)
                    {:keys [sql params]} (sql/generate {:sources srcs :select items :where where :having having
                                                        :group group :order order :limit (:limit q)
                                                        :params (:params q)})
                    rule-ids (into (sorted-set-by #(compare (str %1) (str %2))) (mapcat :rule-ids) srcs)
                    field-rules (into {} (mapcat :field-rules) srcs)
                    user-checks (distinct (for [[_ {:keys [level rule ir]}] (sort-by (comp str key) field-rules)
                                                :when (= :user level)]
                                            [rule ir]))
                    rule-irs (concat (keep :row-rule srcs) (map :ir (vals field-rules)))
                    pin-ids (concat (map :type srcs) (:links @ctx) (:defs @ctx) (map (comp :binding/id :binding) srcs)
                                    rule-ids)
                    plan {:plan/query q
                          :plan/sources (into {} (for [{:keys [alias type binding path field-rules] :as s} srcs
                                                       :let [needed (into (set (:needed s)) (:rule-needed s))
                                                             pushed (when (= :service (:binding/kind binding))
                                                                      (b/pushdown binding (get filters alias)
                                                                                  #(= :row (get-in field-rules [% :level]))))]]
                                                   [alias (cond-> (merge (into {} (remove (comp #{"binding"} namespace key)) binding)
                                                                         {:kind (:binding/kind binding) :type type :path path
                                                                          :binding (:binding/id binding)
                                                                          :fields (select-keys (:binding/fields binding) needed)})
                                                            (seq (select-keys (:binding/values binding) needed))
                                                            (assoc :values (select-keys (:binding/values binding) needed))
                                                            (:binding/row-cap binding) (assoc :row-cap (:binding/row-cap binding))
                                                            (= :service (:binding/kind binding))
                                                            (assoc :call {:service (:service/name binding)
                                                                          :query (:service/query binding)
                                                                          :args (mapv #(dissoc % :attribute) pushed)}
                                                                   :pushed (mapv #(select-keys % [:attribute :op]) pushed)))]))
                          :plan/sql sql
                          :plan/sql-params params
                          :plan/params (or (:params q) {})
                          :plan/user (into #{} (comp (mapcat nodes) (filter #(= ::r/user (first %))) (map second)) rule-irs)
                          :plan/requires {:fields (into #{} (mapcat #(into (set (:needed %)) (:rule-needed %))) srcs)
                                          :rules (set rule-ids)}
                          :plan/rules {:rows (into #{} (comp (filter :row-rule) (map :type)) srcs)
                                       :fields (into {} (for [[a {:keys [level]}] field-rules] [a level]))}
                          :plan/checks (sql/checks user-checks)
                          :plan/output (mapv column items)
                          :plan/pins (into {} (for [id pin-ids] [id (o/hash-of onto id)]))
                          :plan/compiler version}]
                (assoc plan
                       :plan/id (h/sha256 plan)
                       :plan/compiled-at (or now (java.util.Date.)))))))))))
