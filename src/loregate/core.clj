(ns loregate.core
  "The public library API (SPEC 4.1, 10): compile, run, query and plan-status over a system
  map of adapters. HTTP and other transports call these and nothing else."
  (:refer-clojure :exclude [compile])
  (:require [loregate.compile.core :as c]
            [loregate.config :as config]
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
   :cache    (atom {})})                 ; plan id → plan; ponytail: unbounded, add LRU when it matters

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
        (run/run-plan sys plan req)))))

(defn query
  "Compile and run in one step. The plan is cached by id, not stored."
  [sys {:keys [query user] :as req}]
  (if-not user
    {:error :auth/missing-token}
    (let [plan (compile* sys query)]
      (if (:errors plan)
        plan
        (do (swap! (:cache sys) assoc (:plan/id plan) plan)
            (run/run-plan sys plan req))))))

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
