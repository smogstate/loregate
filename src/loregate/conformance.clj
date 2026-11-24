(ns loregate.conformance
  "Conformance kits (SPEC 4.2): what any adapter of a port must satisfy. Each kit returns a
  vector of failures (strings); empty means it conforms. Plain functions, so they run
  under any test framework. Shipped adapters pass them in test/loregate/conformance_test.clj."
  (:require [malli.core :as m]
            [loregate.auth.port :as auth]
            [loregate.ontology.port :as o]
            [loregate.schema.ontology :as ont]
            [loregate.schema.result :as result]
            [loregate.sources.port :as src]
            [loregate.store.port :as store]))

(def ^:private hash-re #"sha256:[0-9a-f]{64}")

(defn- check [failures ok? msg] (cond-> failures (not ok?) (conj msg)))

(defn ontology-kit
  "An Ontology adapter over a valid ontology with at least one type."
  [onto]
  (let [ts (o/types onto)
        ids (map :type/id ts)]
    (as-> [] f
      (check f (and (seq ts) (every? #(and (qualified-keyword? (:type/id %)) (string? (:type/title %))) ts))
             "types: a non-empty list of {:type/id :type/title}")
      (check f (= ids (sort-by str ids)) "types: sorted by id")
      (check f (nil? (o/type-of onto :loregate.conformance/nope)) "type-of: nil for an unknown id")
      (check f (nil? (o/hash-of onto :loregate.conformance/nope)) "hash-of: nil for an unknown id")
      (reduce (fn [f id]
                (let [t (o/type-of onto id)]
                  (-> f
                      (check (m/validate ont/Type (select-keys t (map first (drop 2 ont/Type)))) (str "type-of " id ": a valid type declaration"))
                      (check (every? #(vector? (get t %)) [:type/links :type/defs :type/bindings :type/rules :type/commands])
                             (str "type-of " id ": links, defs, bindings, rules, commands as vectors"))
                      (check (and (re-matches hash-re (str (o/hash-of onto id))) (= (o/hash-of onto id) (o/hash-of onto id)))
                             (str "hash-of " id ": a stable sha256")))))
              f ids)
      (check f (every? #(and (#{:type :attribute :measure} (:kind %)) (qualified-keyword? (:id %))) (o/search onto ""))
             "search: candidates with :kind and :id"))))

(defn store-kit
  "A PlanStore adapter. `make` returns a fresh, empty store; `plan` and `other` are two
  compiled plans."
  [make plan other]
  (let [s (make)
        id (:plan/id plan)
        named (store/set-name! s "sha256:loregate-conformance-unknown" {:name "x"})
        keeps-names? (not= {:error :store/not-supported} named)]
    (store/put-plan! s plan)
    (store/put-plan! s plan)
    (store/put-plan! s other)
    (as-> [] f
      (if (= {} (select-keys (store/names s) ["x"])) f (conj f "set-name!: an unknown plan is never named"))
      (if keeps-names?
        (-> f
            (check (= {:error :plan/not-found} named) "set-name!: {:error :plan/not-found} for an unknown id")
            (check (= (dissoc plan :plan/compiled-at) (dissoc (store/get-plan s id) :plan/compiled-at)) "get-plan: returns what put-plan! stored")
            (check (every? #(contains? (store/dependents s %) id) (keys (:plan/pins plan))) "dependents: every pinned declaration leads to the plan")
            (check (do (store/set-name! s id {:name "kit" :description "d"})
                       (store/set-name! s (:plan/id other) {:name "kit" :description "moved"})
                       (= {"kit" {:plan/id (:plan/id other) :description "moved"}} (store/names s)))
                   "set-name!: a held name moves to its new plan")
            (check (do (store/set-name! s (:plan/id other) nil) (= {} (store/names s))) "set-name!: nil unnames"))
        (check f (= {} (store/names s)) "names: empty when the store keeps no names")))))

(defn source-kit
  "A Source adapter. `binding` is a binding of its kind, `source` the plan source compiled
  from it, `user` a user context. Fetch adapters are called once."
  [adapter binding source user]
  (let [k (src/kind adapter)
        rel (src/relation adapter source)]
    (as-> [] f
      (check f (= k (:binding/kind binding)) "kind: the binding's :binding/kind")
      (check f (nil? (src/check adapter binding)) "check: nil for a valid binding")
      (check f (every? map? (src/check adapter (assoc binding :binding/fields {}))) "check: errors as data")
      (check f (= (src/schema-hash adapter binding) (src/schema-hash adapter binding)) "schema-hash: stable")
      (check f (or (string? (:scan rel)) (= {:fetch true} rel)) "relation: {:scan string} or {:fetch true}")
      (if (:fetch rel)
        (let [res (src/fetch adapter source {} user)]
          (check f (or (and (vector? (:rows res)) (every? map? (:rows res)) (inst? (:as-of res)))
                       (contains? result/run-errors (:error res)))
                 "fetch: {:rows [maps] :as-of inst} or a known run error"))
        f))))

(defn identity-kit
  "An Identity adapter. `valid` is a request it accepts; `invalid` requests it must refuse."
  [identity valid invalid]
  (let [u (auth/authenticate identity valid)]
    (as-> [] f
      (check f (m/validate auth/User u) "authenticate: a valid user context for a valid request")
      (check f (contains? (get-in u [:user/attrs :principals]) (str "user:" (:user/id u))) "principals: include user:<id>")
      (check f (every? #(= "auth" (namespace (:error (auth/authenticate identity %)))) invalid)
             "authenticate: {:error :auth/…} for every invalid request")
      (check f (= {:error :auth/missing-token} (auth/authenticate identity {:headers {}})) "no token: :auth/missing-token"))))

(defn redaction-kit
  "The security gate (SPEC 14.3) through an adapter: for each query and user, running with
  rules equals running without rules over data redacted by hand for that user."
  [{:keys [queries users run-with-rules run-open]}]
  (vec (for [q queries, u users
             :let [a (run-with-rules q u) b (run-open q u)]
             :when (not= a b)]
         (str "redaction: " u " " (pr-str q)))))
