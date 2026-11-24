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
    {:alias (second col) :attr (nth col 2) :op op}))

(defn- accepts? [binding {:keys [attr op]}]
  (contains? (get-in binding [:binding/accepts attr :ops]) op))

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
