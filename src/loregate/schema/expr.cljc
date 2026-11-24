(ns loregate.schema.expr
  "The closed expression language (SPEC 6.3) as malli schemas.

  Three variants share one grammar:
  - query: operands may be `[:param k]`
  - rule:  operands may be `[:user k]`
  - def:   neither (ontology definitions take no params and see no user)

  Syntax only. Types, aggregate placement and name resolution are compile checks.")

;; operator -> argument shape. :n = exactly n args, [:min n] = at least n.
(def operators
  {:= 2 :not= 2 :< 2 :<= 2 :> 2 :>= 2
   :in 2 :between 3 :is-null 1
   :overlaps 2 :subset 2
   :and [:min 1] :or [:min 1] :not 1
   :+ [:min 2] :- [:min 2] :* [:min 2] :/ [:min 2]
   :if 3 :coalesce [:min 2]
   :lower 1 :contains 2 :starts-with 2
   :now 0 :today 0 :days-between 2
   :sum 1 :count [:range 0 1] :count-distinct 1 :avg 1 :min 1 :max 1})

;; :date-trunc takes a unit, then an operand; kept apart from the generic shapes.
(def date-units [:day :week :month :quarter :year])

(def reserved
  "Unqualified keywords a local name may not use."
  (into #{:param :user :date-trunc} (keys operators)))

(defn kind
  "Syntactic kind of an expression node; the :multi dispatch."
  [x]
  (cond
    (and (vector? x) (simple-keyword? (first x))) (first x)
    (vector? x)            ::path
    (set? x)               ::set
    (qualified-keyword? x) ::ref
    (simple-keyword? x)    ::local
    :else                  ::literal))

(def Scalar [:or string? number? boolean? qualified-keyword?])
(def Ref qualified-keyword?)
(def Path [:vector {:min 2} qualified-keyword?])
(def Local [:and simple-keyword? [:fn {:error/message "reserved word"} (complement reserved)]])
(def Selectable [:or Ref Path Local])

(defn- op-schema
  "Fixed arity: a tuple. Variadic: validated as a sequence ([:schema …] keeps the recursive
  ref out of the regex) and generated as a tuple of its required args over `gen` — a
  tuple-only copy of the grammar — because malli's generator cannot bottom out a recursive
  ref inside a sequence. `gen` nil means we are building that copy."
  [op shape self gen]
  (let [tuple (fn [ref n] (into [:tuple [:= op]] (repeat n [:ref ref])))
        [k n] (when (vector? shape) shape)]
    (cond
      (int? shape) (tuple self shape)
      (nil? gen)   (tuple self n)
      :else (-> (into [:cat {:gen/schema [:schema {:registry (:registry gen)} (tuple (:self gen) n)]}
                       [:= op]]
                      (repeat n [:schema [:ref self]]))
                (conj [(if (= k :min) :* :?) [:schema [:ref self]]])))))

(defn- registry
  ([self opts] (registry self opts false))
  ([self {:keys [param? user?] :as opts} gen-only?]
   (let [gen-self (keyword (namespace self) (str (name self) "-gen"))
         gen      (when-not gen-only? {:self gen-self :registry (registry gen-self opts true)})]
     {self (into [:multi {:dispatch kind}]
                 (concat
                  (for [[op shape] operators] [op (op-schema op shape self gen)])
                  [[:date-trunc [:tuple [:= :date-trunc] (into [:enum] date-units) [:ref self]]]
                   [::path Path]
                   [::set [:set {:min 1} Scalar]]
                   [::ref Ref]
                   [::local Local]
                   [::literal [:or string? number? boolean?]]]
                  (when param? [[:param [:tuple [:= :param] simple-keyword?]]])
                  (when user?  [[:user [:tuple [:= :user] simple-keyword?]]])))})))

(def Expr     [:schema {:registry (registry ::expr {:param? true})} ::expr])
(def RuleExpr [:schema {:registry (registry ::rule-expr {:user? true})} ::rule-expr])
(def DefExpr  [:schema {:registry (registry ::def-expr {})} ::def-expr])
