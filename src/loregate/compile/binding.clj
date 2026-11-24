(ns loregate.compile.binding
  "Binding selection (SPEC 5.3). Deterministic: keep the bindings of a registered kind that
  cover every needed attribute and whose required filters the query supplies; take the
  one accepting most of the query's filters; then higher :binding/priority; then id."
  (:require [loregate.compile.resolve :as r]
            [loregate.schema.ontology :as ont]))

(def ^:private pushable (set ont/pushable-ops))
(def ^:private value-node? #{::r/lit ::r/lit-set ::r/enum ::r/param})

(defn- conjuncts [ir]
  (cond (nil? ir) []
        (= :and (first ir)) (mapcat conjuncts (rest ir))
        :else [ir]))

(defn filters
  "Top-level :where conjuncts of the form [op [col alias attr] value…]: what a source could
  apply itself."
  [where-ir]
  (for [[op col & vs] (conjuncts where-ir)
        :when (and (pushable op) (vector? col) (= ::r/col (first col))
                   (seq vs) (every? #(value-node? (first %)) vs))]
    {:alias (second col) :attr (nth col 2) :op op :values (vec vs)}))

(defn- accepts? [binding {:keys [attr op]}]
  (contains? (get-in binding [:binding/accepts attr :ops]) op))

(defn- arg-value
  "A pushed value as a descriptor: {:value v} (enum values in their stored form) or
  {:param k}; a literal set becomes a vector of them."
  [binding attr node]
  (case (first node)
    ::r/lit     {:value (second node)}
    ::r/enum    {:value (get-in binding [:binding/values attr (second node)])}
    ::r/param   {:param (second node)}
    ::r/lit-set (mapv #(if (qualified-keyword? %)
                         {:value (get-in binding [:binding/values attr %])}
                         {:value %})
                      (sort-by str (second node)))))

(defn pushdown
  "The filters a binding applies itself (SPEC 7 step 6): accepted attribute and operator,
  no row-level field rule on the attribute (8.3), one per argument. A pushed conjunct also
  stays in the SQL, where it changes nothing. → [{:arg :op :attribute :value}]."
  [binding source-filters row-level?]
  (->> source-filters
       (filter #(and (accepts? binding %) (not (row-level? (:attr %)))))
       (reduce (fn [acc {:keys [attr op values]}]
                 (let [arg (get-in binding [:binding/accepts attr :arg])]
                   (if (some #(= arg (:arg %)) acc)
                     acc
                     (conj acc {:arg arg :op op :attribute attr
                                :value (if (#{:in :between} op)
                                         (vec (mapcat #(let [v (arg-value binding attr %)] (if (vector? v) v [v])) values))
                                         (arg-value binding attr (first values)))}))))
               [])))

(defn choose
  "The binding for one source, or nil."
  [bindings needed source-filters kinds]
  (let [score #(count (filter (partial accepts? %) source-filters))]
    (->> bindings
         (filter #(contains? kinds (:binding/kind %)))
         (filter #(every? (set (keys (:binding/fields %))) needed))
         (filter (fn [b] (every? (fn [a] (some #(and (= a (:attr %)) (accepts? b %)) source-filters))
                                 (:binding/required b))))
         (sort-by (juxt (comp - score) #(- (:binding/priority % 0)) (comp str :binding/id)))
         first)))
