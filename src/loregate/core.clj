(ns loregate.core
  "The public library API (SPEC 4.1, 10): compile, run, query and plan-status over a system
  map of adapters. HTTP and other transports call these and nothing else."
  (:refer-clojure :exclude [compile])
  (:require [loregate.compile.core :as c]
            [loregate.config :as config]
            [loregate.ontology.port :as o]
            [loregate.run.core :as run]
            [loregate.store.none :as none]
            [loregate.store.port :as store]))

(defn system
  "A system map. Required: :ontology (an Ontology) and :sources {kind adapter}. Optional:
  :store (default: none) and :config (merged over loregate.config/defaults)."
  [{:keys [ontology sources store config]}]
  {:ontology ontology
   :sources  sources
   :store    (or store (none/store))
   :config   (merge config/defaults config)
   :cache    (atom {})
   :busy     (atom {})})                 ; user id → runs in progress                 ; plan id → plan; ponytail: unbounded, add LRU when it matters

(defn- compile* [sys query]
  (c/compile-query (:ontology sys) query {:sources (:sources sys)
                                          :max-limit (get-in sys [:config :max-limit])}))

(defn inputs
  "What a plan reads, per source: [{:path [links…] :type t :attributes #{…}}], ordered by
  path ([] is the :from type). Ontology terms only. The keys of :relations are these paths."
  [plan]
  (->> (vals (:plan/sources plan))
       (map (fn [{:keys [path type fields]}] {:path path :type type :attributes (set (keys fields))}))
       (sort-by (juxt (comp count :path) (comp str :path)))
       vec))

(defn- summary [plan]
  (assoc (select-keys plan [:plan/id :plan/output :plan/params]) :plan/inputs (inputs plan)))

(defn compile
  "Compile and keep a plan: {:plan/id :plan/output :plan/params}, or {:errors […]}."
  [sys query]
  (let [plan (compile* sys query)]
    (if (:errors plan)
      plan
      (do (store/put-plan! (:store sys) plan)
          (swap! (:cache sys) assoc (:plan/id plan) plan)
          (summary plan)))))

(defn- load-plan
  "From the cache, else the store, else by recompiling the query the caller sent."
  [sys plan-id query]
  (or (get @(:cache sys) plan-id)
      (when-let [p (store/get-plan (:store sys) plan-id)]
        (swap! (:cache sys) assoc plan-id p)
        p)
      (when query
        (let [p (compile* sys query)]
          (cond
            (:errors p) p
            (= plan-id (:plan/id p)) (do (swap! (:cache sys) assoc plan-id p) p)
            :else {:error :plan/stale :changed [] :recompiled (:plan/id p)})))
      {:error :plan/not-found}))

(defn- budgeted
  "Run `f` within the user's concurrency limit and the run time budget (SPEC 9)."
  [sys user f]
  (let [{:keys [max-concurrent-runs run-timeout-ms]} (:config sys)
        id (:user/id user)
        busy (:busy sys)]
    (if (> (get (swap! busy update id (fnil inc 0)) id) max-concurrent-runs)
      (do (swap! busy update id dec) {:error :budget/busy})
      (try
        ;; ponytail: on timeout the abandoned run finishes in the background; cancel the
        ;; DuckDB statement when that load matters
        (let [fut (future (f))
              res (deref fut run-timeout-ms ::timeout)]
          (if (= ::timeout res)
            (do (future-cancel fut) {:error :source/timeout})
            res))
        (finally (swap! busy update id dec))))))

(defn run
  "Run a plan for a user: rows + notes, or {:error …}. `req`: :plan-id, :params, :user,
  optionally :query (used when the plan is not held). Library only: :relations (rows per
  source path, as in :plan/inputs, in ontology names) and :clock ({:now inst :zone \"…\"})."
  [sys {:keys [plan-id query user] :as req}]
  (if-not user
    {:error :auth/missing-token}
    (let [plan (load-plan sys plan-id query)]
      (if (or (:error plan) (:errors plan))
        plan
        (budgeted sys user #(run/run-plan sys plan req))))))

(defn query
  "Compile and run in one step. The plan is cached by id, not stored."
  [sys {:keys [query user] :as req}]
  (if-not user
    {:error :auth/missing-token}
    (let [plan (compile* sys query)]
      (if (:errors plan)
        plan
        (do (swap! (:cache sys) assoc (:plan/id plan) plan)
            (budgeted sys user #(run/run-plan sys plan req)))))))

(defn- visible? [sys type attr user]
  (let [{:keys [level checks]} (c/field-check (:ontology sys) type attr)]
    (case level
      nil true
      :row :per-row
      :user (boolean (some-> (seq (:rules checks)) first ((run/evaluate-checks checks user)))))))

(defn- describe-type [sys id user]
  (when-let [t (o/type-of (:ontology sys) id)]
    {:type/id id :type/title (:type/title t) :type/owner (:type/owner t)
     :type/definition (:type/definition t) :type/key (:type/key t)
     :attributes (vec (for [[a decl] (sort-by (comp str key) (:type/attributes t))]
                        (assoc (select-keys decl [:type :of :values :unit :nullable?])
                               :id a :visible? (visible? sys id a user))))
     :links (mapv #(select-keys % [:link/id :link/to :link/cardinality]) (:type/links t))
     :definitions (mapv #(select-keys % [:def/id :def/kind :def/type :def/unit :def/owner]) (:type/defs t))
     :commands (mapv #(select-keys % [:command/id :command/params :command/service :command/endpoint])
                     (:type/commands t))}))

(defn describe
  "Types, attributes (with :visible? for `user`: true, false or :per-row), links,
  definitions and commands (SPEC 10). Never bindings or rules. `req`: :types (ids) or
  :search (text), and :user. With neither, every type."
  [sys {:keys [types search user]}]
  (if-not user
    {:error :auth/missing-token}
    (let [onto (:ontology sys)
          matches (when search (o/search onto search))
          ids (cond (seq types) types
                    search (distinct (map #(if (= :type (:kind %)) (:id %) (:on %)) matches))
                    :else (map :type/id (o/types onto)))]
      (cond-> {:types (vec (keep #(describe-type sys % user) ids))
               :named (vec (for [[n {:keys [description] :as e}] (sort-by key (store/names (:store sys)))
                                 :let [p (store/get-plan (:store sys) (:plan/id e))]
                                 :when p]
                             {:name n :description description :plan/id (:plan/id p)
                              :plan/output (:plan/output p) :plan/params (:plan/params p)}))}
        search (assoc :matches matches)))))

(def ^:private name-re #"[A-Za-z0-9_-]{1,64}")   ; tool-name rules, so a name can become an agent tool

(defn- may-publish? [sys user]
  (let [role (get-in sys [:config :publish/role])]
    (and role (contains? (set (get-in user [:user/attrs :roles])) role))))

(defn publish
  "Give a compiled plan a stable name and description (SPEC 10). Needs the role in
  :publish/role and a store that keeps names."
  [sys {:keys [plan-id name description user]}]
  (cond
    (not user) {:error :auth/missing-token}
    (not (may-publish? sys user)) {:error :auth/forbidden}
    (not (and (string? name) (re-matches name-re name))) {:error :request/invalid :reason :bad-name}
    :else (or (store/set-name! (:store sys) plan-id {:name name :description description})
              {:name name :plan/id plan-id})))

(defn unpublish
  "Remove a plan's name."
  [sys {:keys [plan-id user]}]
  (cond
    (not user) {:error :auth/missing-token}
    (not (may-publish? sys user)) {:error :auth/forbidden}
    :else (or (store/set-name! (:store sys) plan-id nil) {:plan/id plan-id})))

(defn run-named
  "Run a named plan: like run, by name."
  [sys {:keys [name] :as req}]
  (if-let [id (get-in (store/names (:store sys)) [name :plan/id])]
    (run sys (assoc req :plan-id id))
    {:error :plan/not-found}))

(defn plan-status
  "Output, params, pins and staleness of a plan. Depends on no user's rights."
  [sys {:keys [plan-id query]}]
  (let [plan (load-plan sys plan-id query)]
    (if (or (:error plan) (:errors plan))
      plan
      (let [changed (run/stale (:ontology sys) plan)]
        (assoc (select-keys plan [:plan/id :plan/output :plan/params :plan/pins])
               :plan/inputs (inputs plan)
               :stale? (boolean (seq changed))
               :changed changed)))))
