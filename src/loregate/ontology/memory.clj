(ns loregate.ontology.memory
  "In-memory ontology adapter over a vector of declarations (an EDN file in practice).
  Refuses to start on an invalid ontology: every problem is reported, none is skipped."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [loregate.hash :as h]
            [loregate.ontology.port :as port]
            [loregate.schema.ontology :as s]))

(defn- id-of [decl]
  (case (s/kind decl)
    :type (:type/id decl) :link (:link/id decl) :def (:def/id decl)
    :binding (:binding/id decl) :command (:command/id decl) :rule (s/rule-id decl)))

(defn- schema-problems [decls]
  (keep-indexed (fn [i d] (when-let [e (m/explain s/Declaration d)]
                            {:problem :invalid-declaration :index i :details (me/humanize e)}))
                decls))

(defn- reference-problems
  "Cross-declaration checks. Assumes every declaration passed the schema."
  [decls]
  (let [by-kind (group-by s/kind decls)
        types   (into {} (map (juxt :type/id identity)) (:type by-kind))
        attrs-of (fn [t] (set (keys (get-in types [t :type/attributes]))))
        attr-decls (group-by first (for [t (:type by-kind) [a d] (:type/attributes t)] [a d]))
        p (fn [problem & kvs] (apply hash-map :problem problem kvs))]
    (concat
     (for [[id ds] (group-by id-of decls) :when (> (count ds) 1)]
       (p :duplicate-id :id id))
     (for [[a ds] attr-decls :when (> (count (set (map second ds))) 1)]
       (p :attribute-conflict :attribute a))
     (for [t (:type by-kind), k (:type/key t) :when (not ((attrs-of (:type/id t)) k))]
       (p :unknown-attribute :in (:type/id t) :attribute k))
     (for [{:link/keys [id from to on]} (:link by-kind)
           problem (concat
                    (for [t [from to] :when (not (types t))] (p :unknown-type :in id :type t))
                    (when (and (types from) (types to))
                      (for [[fa ta] on
                            [t a] [[from fa] [to ta]]
                            :when (not ((attrs-of t) a))]
                        (p :unknown-attribute :in id :attribute a))))]
       problem)
     (for [{:def/keys [id on]} (:def by-kind)
           problem [(when-not (types on) (p :unknown-type :in id :type on))
                    (when ((attrs-of on) id) (p :def-shadows-attribute :in id))]
           :when problem]
       problem)
     (for [{:binding/keys [id type fields values accepts required]} (:binding by-kind)
           problem (if-not (types type)
                     [(p :unknown-type :in id :type type)]
                     (let [stored (get-in types [type :type/attributes])
                           enum?  #(= :enum (get-in stored [% :type]))]
                       (concat
                        (for [a (keys fields) :when (not (stored a))]
                          (p :unknown-attribute :in id :attribute a))
                        (for [a (keys fields) :when (and (enum? a) (not (get values a)))]
                          (p :enum-without-values :in id :attribute a))
                        (for [[a vs] values
                              :when (or (not (enum? a))
                                        (not= (set (keys vs)) (get-in stored [a :values])))]
                          (p :bad-values :in id :attribute a))
                        (for [a (keys accepts) :when (not (contains? fields a))]
                          (p :accepts-unmapped :in id :attribute a))
                        (for [a required :when (not (contains? accepts a))]
                          (p :required-not-accepted :in id :attribute a)))))]
       problem)
     (for [{:rule/keys [kind on]} (:rule by-kind)
           :when (case kind :rows (not (types on)) :field (not (attr-decls on)))]
       (p :unknown-rule-target :rule [kind on]))
     (for [{:command/keys [id on]} (:command by-kind) :when (not (types on))]
       (p :unknown-type :in id :type on)))))

(defn problems
  "Every problem in `decls`, or nil when the ontology is valid."
  [decls]
  (seq (or (seq (schema-problems decls))
           (reference-problems decls))))

(defn- matches? [needle & haystacks]
  (some #(and % (str/includes? (str/lower-case (str %)) needle)) haystacks))

(defrecord Memory [decls types-by-id by-kind by-id schema-hash]
  port/Ontology
  (types [_]
    (->> (vals types-by-id)
         (map #(select-keys % [:type/id :type/title]))
         (sort-by :type/id)))

  (type-of [_ id]
    (when-let [t (types-by-id id)]
      (let [attrs (set (keys (:type/attributes t)))]
        (assoc t
               :type/links    (filterv #(= id (:link/from %)) (:link by-kind))
               :type/defs     (filterv #(= id (:def/on %)) (:def by-kind))
               :type/bindings (filterv #(= id (:binding/type %)) (:binding by-kind))
               :type/rules    (filterv #(case (:rule/kind %)
                                          :rows  (= id (:rule/on %))
                                          :field (attrs (:rule/on %)))
                                       (:rule by-kind))
               :type/commands (filterv #(= id (:command/on %)) (:command by-kind))))))

  (search [_ text]
    (let [needle (str/lower-case text)]
      (->> (concat
            (for [t (vals types-by-id)
                  :when (matches? needle (:type/id t) (:type/title t) (:type/definition t))]
              {:kind :type :id (:type/id t)})
            (for [t (vals types-by-id), a (keys (:type/attributes t)) :when (matches? needle a)]
              {:kind :attribute :id a :on (:type/id t)})
            (for [d (:def by-kind) :when (matches? needle (:def/id d))]
              {:kind (:def/kind d) :id (:def/id d) :on (:def/on d)}))
           (sort-by (juxt (comp name :kind) (comp str :id) (comp str :on)))
           vec)))

  (hash-of [_ id]
    (when-let [d (by-id id)]
      (if (and schema-hash (= :binding (s/kind d)))
        (h/sha256 [d (schema-hash d)])
        (h/sha256 d)))))

(defn create
  "An Ontology over `decls`. Throws ex-info {:error :ontology/invalid :problems […]} when
  the declarations are invalid. Option :schema-hash, a fn of a binding, folds the source
  adapter's schema into the binding's hash (SPEC 5.6)."
  ([decls] (create decls {}))
  ([decls {:keys [schema-hash]}]
   (when-let [ps (problems decls)]
     (throw (ex-info "Invalid ontology" {:error :ontology/invalid :problems (vec ps)})))
   (let [by-kind (group-by s/kind decls)]
     (->Memory decls
               (into {} (map (juxt :type/id identity)) (:type by-kind))
               by-kind
               (into {} (map (juxt id-of identity)) decls)
               schema-hash))))

(defn load-edn
  "Read a vector of declarations from an EDN file or resource path."
  [path]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (edn/read r)))
