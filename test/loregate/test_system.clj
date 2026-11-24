(ns loregate.test-system
  "Shared test setup: a system over the fixture ontology and Parquet files, a fixed clock,
  and the fixture data in ontology names (for supplied relations)."
  (:require [loregate.core :as lg]
            [loregate.fixtures :as fx]
            [loregate.ontology.memory :as mem]
            [loregate.sources.parquet :as pq]))

(def clock {:now #inst "2026-11-24T12:00:00Z" :zone "UTC"})
(def admin
  "Sees every row and field the fixture rules allow anyone to see, for engine tests that
  are not about rules. Rule tests use the fixture users (dev/fixtures/users.edn)."
  {:user/id "admin"
   :user/attrs {:roles #{"role:finance" "role:crm-admin" "role:legal"} :region "EU"
                :principals #{"user:admin" "group:legal-us" "group:legal-eu"}}})

(defn system
  ([] (system {}))
  ([{:keys [decls sources config store]}]
   (lg/system {:ontology (mem/create (or decls (fx/ontology)))
               :sources (or sources {:parquet (pq/adapter ["dev/fixtures/"])})
               :config config
               :store store})))

(defn run-query
  "Compile and run with the fixed clock; rows or the error."
  [sys q & {:as opts}]
  (let [res (lg/query sys (merge {:query q :user admin :clock clock} opts))]
    (if (:rows res) (:rows res) res)))

(defn ontology-rows
  "data.edn rows of the table behind `binding-id`, in ontology names and values."
  [binding-id]
  (let [b (some #(when (= binding-id (:binding/id %)) %) (fx/ontology))
        table (keyword (re-find #"[a-z]+(?=\.parquet$)" (:parquet/path b)))
        {:keys [columns rows]} (get (fx/data) table)
        col-names (map first columns)]
    (for [row rows
          :let [stored (zipmap col-names row)]]
      (into {} (for [[attr field] (:binding/fields b)
                     :when (string? field)
                     :let [v (get stored field)
                           back (some-> (get-in b [:binding/values attr]) (->> (map (juxt val key)) (into {})))]]
                 [attr (if back (get back v) v)])))))
