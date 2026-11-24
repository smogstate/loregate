(ns loregate.compile.resolve
  "Names → IR (SPEC 7, steps 2–3).

  Every reference becomes [::col alias attribute]. Derived attributes, measures and :let
  locals are expanded in place; a measure's :def/where becomes the filter of each
  aggregate inside it. IR nodes:

    [::col alias attr]          a stored attribute of a source
    [::lit v]                   a literal; [::lit-set #{…}] a literal set
    [::enum kw]                 an enum value
    [::param k]  [::user k]     parameters; user attributes (rules only)
    [::def decl ir]             an expanded definition, kept for its declared type
    [::agg op filter & args]    an aggregate; filter is IR or nil
    [op & args]                 any other operator, as in the DSL

  Sources are keyed by their path of link ids from the :from type ([] for the root); each
  gets an alias the first time it is reached, so aliases follow the query's text order."
  (:require [clojure.string :as str]
            [loregate.ontology.port :as o]
            [loregate.schema.expr :as expr]))

(def aggregates #{:sum :count :count-distinct :avg :min :max})

(defn new-ctx [onto query]
  (atom {:onto onto :query query
         :sources {} :order [] :errors []
         :types-cache {} :defs #{} :links #{}}))

(defn err! [ctx at error & kvs]
  (swap! ctx update :errors conj (merge {:error error :at at} (apply hash-map kvs)))
  ::error)

(defn type-info [ctx t]
  (or (get-in @ctx [:types-cache t])
      (let [ti (o/type-of (:onto @ctx) t)]
        (swap! ctx assoc-in [:types-cache t] ti)
        ti)))

(defn- alias-for [ctx path type]
  (let [base (str/replace (name type) #"[^a-zA-Z0-9]" "_")
        taken (set (map :alias (vals (:sources @ctx))))]
    (->> (cons base (map #(str base "_" %) (iterate inc 2)))
         (map keyword)
         (remove taken)
         first)))

(defn source!
  "The alias of the source at `path`, registering it on first use."
  [ctx path type link]
  (or (get-in @ctx [:sources path :alias])
      (let [a (alias-for ctx path type)]
        (swap! ctx #(-> %
                        (assoc-in [:sources path] {:alias a :type type :path path :link link
                                                   :parent (when (seq path) (pop path))
                                                   :needed #{}})
                        (update :order conj path)))
        a)))

(defn need!
  "Record that the source at `path` must provide `attr`; return the column node."
  [ctx path attr]
  (swap! ctx update-in [:sources path :needed] conj attr)
  [::col (get-in @ctx [:sources path :alias]) attr])

(defn- enum-value? [ctx kw]
  ;; any enum attribute of any type reached so far, or declared at all
  (some (fn [{:type/keys [id]}]
          (some #(contains? (:values %) kw) (vals (:type/attributes (type-info ctx id)))))
        (o/types (:onto @ctx))))

(defn- and* [a b] (cond (nil? a) b (nil? b) a :else [:and a b]))

(declare resolve-expr)

(defn- resolve-attr
  "A qualified keyword in the scope of a source: stored attribute, derived attribute or
  measure of that source's type, or an enum value."
  [ctx {:keys [path type stack] :as scope} at kw]
  (let [ti (type-info ctx type)
        d  (some #(when (= kw (:def/id %)) %) (:type/defs ti))]
    (cond
      (contains? (:type/attributes ti) kw)
      (need! ctx path kw)

      d
      (if (contains? stack kw)
        (err! ctx at :compile/cycle :name kw)
        (let [scope' (-> scope (update :stack conj kw) (assoc :locals nil))] ; defs never see query locals
          (swap! ctx update :defs conj kw)
          (case (:def/kind d)
            :attribute [::def d (resolve-expr ctx scope' at (:def/expr d))]
            :measure
            (if (or (seq path) (not (:measures? scope)))
              (err! ctx at :compile/measure-not-on-from :name kw)
              (let [w (some->> (:def/where d) (resolve-expr ctx (assoc scope' :filter nil) at))]
                [::def d (resolve-expr ctx (update scope' :filter and* w) at (:def/expr d))])))))

      (enum-value? ctx kw)
      [::enum kw]

      :else
      (err! ctx at :compile/unknown-name :name kw))))

(defn- resolve-path [ctx {:keys [path type] :as scope} at steps]
  (loop [path path, type type, [l & more] (butlast steps)]
    (if (nil? l)
      (resolve-attr ctx (assoc scope :path path :type type :measures? false) at (last steps))
      (let [link (some #(when (= l (:link/id %)) %) (:type/links (type-info ctx type)))]
        (cond
          (and (nil? link) (nil? (o/hash-of (:onto @ctx) l)))
          (err! ctx at :compile/unknown-name :name l)

          (or (nil? link) (= :one-to-many (:link/cardinality link)))
          (err! ctx at :compile/bad-path :name l)

          :else
          (let [path' (conj path l)]
            (swap! ctx update :links conj l)
            (source! ctx path' (:link/to link) link)
            ;; the join reads the link's attributes on both sides
            (doseq [[from-a to-a] (:link/on link)]
              (need! ctx path from-a)
              (need! ctx path' to-a))
            (recur path' (:link/to link) more)))))))

(defn resolve-expr
  "IR for expression `x` in `scope` {:path :type :stack :filter :measures? :locals}."
  [ctx scope at x]
  (let [k (expr/kind x)
        sub #(resolve-expr ctx scope at %)]
    (case k
      ::expr/ref     (resolve-attr ctx scope at x)
      ::expr/path    (resolve-path ctx scope at x)
      ::expr/literal [::lit x]
      ::expr/set     [::lit-set x]
      ::expr/local   (let [{:keys [locals stack]} scope
                           lx (get locals x)]
                       (cond
                         (nil? lx)            (err! ctx at :compile/unknown-name :name x)
                         (contains? stack x)  (err! ctx at :compile/cycle :name x)
                         :else (resolve-expr ctx (update scope :stack conj x) at lx)))
      :param         [::param (second x)]
      :user          [::user (second x)]
      :date-trunc    [:date-trunc (second x) (sub (nth x 2))]
      (if (aggregates k)
        (into [::agg k (:filter scope)] (map sub (rest x)))
        (into [k] (map sub (rest x)))))))
