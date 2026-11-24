(ns loregate.compile.typecheck
  "Types for IR (SPEC 7, step 4). `check` returns [ir' type]: literals are rewritten to
  [::r/lit v target] so the SQL can cast each parameter to the type its context expects.

  Types: {:type :string|:int|:decimal|:boolean|:date|:datetime}, {:type :enum :values #{}},
  {:type :set :of t}, plus the literal-only {:type :enum-value :value kw} and
  {:type :lit-set :elems #{}}, and {:type :user} for [:user k] (rules). :agg? marks a type
  computed by an aggregate; :literal holds a literal's value."
  (:require [loregate.compile.resolve :as r])
  (:import (java.time LocalDate LocalDateTime)))

(def numeric #{:int :decimal})
(def ordered #{:int :decimal :string :date :datetime})
(def error-type {:type ::error})

(defn- parses? [target s]
  (try (case target
         :date (LocalDate/parse s)
         :datetime (LocalDateTime/parse s))
       true
       (catch Exception _ false)))

(defn- literal-type [v]
  (cond (string? v)  {:type :string :literal v}
        (boolean? v) {:type :boolean :literal v}
        (integer? v) {:type :int :literal v}
        (number? v)  {:type :decimal :literal v}))

(defn- coerce
  "Retarget a literal to the type it is compared with, when that is sound."
  [[ir t] target]
  (let [tt (:type target)]
    (if (and (= ::r/lit (first ir)) (contains? t :literal))
      (cond
        (and (#{:date :datetime} tt) (string? (:literal t)) (parses? tt (:literal t)))
        [[::r/lit (second ir) tt] (assoc target :literal (:literal t))]
        (and (= :decimal tt) (= :int (:type t)))
        [[::r/lit (second ir) :decimal] (assoc target :literal (:literal t))]
        :else [ir t])
      [ir t])))

(defn comparable? [a b]
  (let [[ta tb] [(:type a) (:type b)]]
    (or (= ::error ta) (= ::error tb)
        (and (numeric ta) (numeric tb))
        (and (#{:date :datetime} ta) (#{:date :datetime} tb))
        (and (= ta tb) (#{:string :boolean} ta))
        (and (= :enum ta) (= :enum tb) (= (:values a) (:values b)))
        (and (= :enum ta) (= :enum-value tb) (contains? (:values a) (:value b)))
        (and (= :enum-value ta) (= :enum tb) (contains? (:values b) (:value a)))
        (and (= :enum-value ta tb)))))

(defn- elem-type [t]
  (case (:type t)
    :set     {:type (:of t)}
    :lit-set nil                      ; checked element by element
    :user    {:type :user}
    nil))

(defn- set-like? [t] (#{:set :lit-set :user ::error} (:type t)))

(defn- widen [a b]
  (cond (= ::error (:type a)) b
        (= ::error (:type b)) a
        (and (numeric (:type a)) (numeric (:type b)))
        {:type (if (= :int (:type a) (:type b)) :int :decimal)}
        :else (dissoc a :literal :agg?)))

(defn- lit-elem-type [v]
  (if (qualified-keyword? v) {:type :enum-value :value v} (literal-type v)))

(defn check
  "[ir' type] for `ir`. Errors go to the resolver ctx; their type is ::error."
  [ctx env at ir]
  (let [{:keys [params alias-types]} env
        err (fn [& kvs] (apply r/err! ctx at :compile/type-mismatch kvs) error-type)
        sub (fn [x] (check ctx env at x))
        agg? (fn [ts] (boolean (some :agg? ts)))
        result (fn [ir t ts] [ir (cond-> t (agg? ts) (assoc :agg? true))])]
    (if (= ::r/error ir)
      [ir error-type]
      (case (first ir)
        ::r/col (let [[_ a attr] ir
                      decl (get-in (r/type-info ctx (alias-types a)) [:type/attributes attr])]
                  [ir (select-keys decl [:type :values :of :unit :scale])])
        ::r/lit (let [t (literal-type (second ir))] [[::r/lit (second ir) (:type t)] t])
        ::r/lit-set [ir {:type :lit-set :elems (second ir)}]
        ::r/enum [ir {:type :enum-value :value (second ir)}]
        ::r/user [ir {:type :user}]
        ::r/param (if-let [p (get params (second ir))]
                    [ir {:type (:type p)}]
                    [ir (do (r/err! ctx at :compile/unknown-param :name (second ir)) error-type)])

        ::r/def (let [[_ d inner] ir
                      [inner' t] (sub inner)
                      declared (cond-> {:type (:def/type d)}
                                 (:def/unit d) (assoc :unit (:def/unit d))
                                 ;; a decimal definition keeps the scale of what it computes
                                 (and (= :decimal (:def/type d) (:type t)) (:scale t)) (assoc :scale (:scale t)))]
                  (if (comparable? t declared)
                    [[::r/def d inner'] (cond-> declared (:agg? t) (assoc :agg? true))]
                    [ir (err :name (:def/id d) :declared (:def/type d) :found (:type t))]))

        ::r/agg (let [[_ op filter & args] ir
                      checked (map sub args)
                      ts (map second checked)
                      [filter' ft] (when filter (sub filter))
                      t (first ts)]
                  (cond
                    (some :agg? ts) [ir (err :reason :nested-aggregate)]
                    (and ft (not (#{:boolean ::error} (:type ft)))) [ir (err :reason :filter-not-boolean)]
                    :else
                    [(into [::r/agg op filter'] (map first checked))
                     (assoc (case op
                              (:count :count-distinct) {:type :int}
                              :avg (if (numeric (:type t)) {:type :decimal} (err :op op :found (:type t)))
                              :sum (if (numeric (:type t)) (select-keys t [:type :unit :scale])
                                       (err :op op :found (:type t)))
                              (:min :max) (if (ordered (:type t)) (dissoc t :literal)
                                              (err :op op :found (:type t))))
                            :agg? true)]))

        (:= :not= :< :<= :> :>=)
        (let [[a b] (map sub (rest ir))
              b (coerce b (second a)) a (coerce a (second b))
              [ta tb] [(second a) (second b)]
              ok (and (comparable? ta tb)
                      (or (#{:= :not=} (first ir))
                          (every? #(or (ordered (:type %)) (= ::error (:type %))) [ta tb])))]
          (result [(first ir) (first a) (first b)]
                  (if ok {:type :boolean} (err :op (first ir) :found [(:type ta) (:type tb)]))
                  [ta tb]))

        :between
        (let [[x lo hi] (map sub (rest ir))
              lo (coerce lo (second x)) hi (coerce hi (second x))
              ts (map second [x lo hi])]
          (result [:between (first x) (first lo) (first hi)]
                  (if (and (every? #(comparable? (second x) %) (map second [lo hi]))
                           (or (ordered (:type (second x))) (= ::error (:type (second x)))))
                    {:type :boolean}
                    (err :op :between :found (map :type ts)))
                  ts))

        :is-null
        (let [[x] (map sub (rest ir))]
          (result [:is-null (first x)] {:type :boolean} [(second x)]))

        :in
        (let [[x coll] (map sub (rest ir))
              [tx tc] [(second x) (second coll)]
              ok (case (:type tc)
                   :lit-set (every? #(comparable? tx (second (coerce [[::r/lit %] (lit-elem-type %)] tx)))
                                    (:elems tc))
                   (:set :user) (comparable? tx (elem-type tc))
                   ::error true
                   false)
              ;; a literal set compared to dates holds date strings: retarget them
              target (when (#{:date :datetime :decimal} (:type tx)) (:type tx))
              coll-ir (if (and target (= :lit-set (:type tc))) [::r/lit-set (second (first coll)) target] (first coll))]
          (result [:in (first x) coll-ir]
                  (if ok {:type :boolean} (err :op :in :found [(:type tx) (:type tc)]))
                  [tx tc]))

        (:overlaps :subset)
        (let [[a b] (map sub (rest ir))]
          (result [(first ir) (first a) (first b)]
                  (if (and (set-like? (second a)) (set-like? (second b)))
                    {:type :boolean}
                    (err :op (first ir) :found [(:type (second a)) (:type (second b))]))
                  [(second a) (second b)]))

        (:and :or :not)
        (let [checked (map sub (rest ir)) ts (map second checked)]
          (result (into [(first ir)] (map first checked))
                  (if (every? #(#{:boolean ::error} (:type %)) ts)
                    {:type :boolean}
                    (err :op (first ir) :found (map :type ts)))
                  ts))

        (:+ :- :* :/)
        (let [checked (map sub (rest ir)) ts (map second checked)]
          (result (into [(first ir)] (map first checked))
                  (cond
                    (not (every? #(#{:int :decimal ::error} (:type %)) ts)) (err :op (first ir) :found (map :type ts))
                    (or (= :/ (first ir)) (some #(= :decimal (:type %)) ts)) {:type :decimal}
                    :else {:type :int})
                  ts))

        :if
        (let [[c a b] (map sub (rest ir))
              b (coerce b (second a)) a (coerce a (second b))
              ts (map second [c a b])]
          (result [:if (first c) (first a) (first b)]
                  (if (and (#{:boolean ::error} (:type (second c))) (comparable? (second a) (second b)))
                    (widen (second a) (second b))
                    (err :op :if :found (map :type ts)))
                  ts))

        :coalesce
        (let [checked (map sub (rest ir))
              t0 (second (first checked))
              checked (map #(coerce % t0) checked)
              ts (map second checked)]
          (result (into [:coalesce] (map first checked))
                  (if (every? #(comparable? t0 %) ts) (reduce widen ts) (err :op :coalesce :found (map :type ts)))
                  ts))

        :lower
        (let [[x] (map sub (rest ir))]
          (result [:lower (first x)]
                  (if (#{:string ::error} (:type (second x))) {:type :string} (err :op :lower :found (:type (second x))))
                  [(second x)]))

        (:contains :starts-with)
        (let [[a b] (map sub (rest ir)) ts (map second [a b])]
          (result [(first ir) (first a) (first b)]
                  (if (every? #(#{:string ::error} (:type %)) ts) {:type :boolean} (err :op (first ir) :found (map :type ts)))
                  ts))

        :now   [ir {:type :datetime}]
        :today [ir {:type :date}]

        :days-between
        (let [[a b] (map sub (rest ir))
              a (coerce a {:type :date}) b (coerce b {:type :date})
              ts (map second [a b])]
          (result [:days-between (first a) (first b)]
                  (if (every? #(#{:date :datetime ::error} (:type %)) ts) {:type :int} (err :op :days-between :found (map :type ts)))
                  ts))

        :date-trunc
        (let [[_ unit x] ir
              [x' t] (sub x)]
          (result [:date-trunc unit x']
                  (if (#{:date :datetime ::error} (:type t)) (dissoc t :literal) (err :op :date-trunc :found (:type t)))
                  [t]))))))
