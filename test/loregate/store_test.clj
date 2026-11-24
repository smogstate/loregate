(ns loregate.store-test
  "Phase 5 gate: changing a fixture measure makes its dependents report stale, and only
  those. Every store adapter honours the same contract."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [loregate.core :as lg]
            [loregate.fixtures :as fx]
            [loregate.ontology.memory :as mem]
            [loregate.run-test :refer [spec-query]]
            [loregate.store.graph :as graph]
            [loregate.store.memory :as memory]
            [loregate.store.port :as store]
            [loregate.test-system :as ts]))

(def contract-query {:from :biz/contract :select [:biz/contract-total]})
(def client-query {:from :biz/client :select [:biz/client-name]})

(defn- temp-db []
  (let [f (java.io.File/createTempFile "loregate-graph" ".db")]
    (.delete f)                                ; DuckDB creates it
    (.deleteOnExit f)
    (str f)))

(deftest staleness-follows-the-graph
  (doseq [[label make] [["memory" (fn [] (memory/store))] ["graph" (fn [] (graph/store (temp-db)))]]]
    (testing label
      (let [s (make)
            sys (ts/system {:store s})
            ids (into {} (for [[k q] {:unpaid spec-query :contract contract-query :client client-query}]
                           [k (:plan/id (lg/compile sys q))]))
            changed (mapv #(if (= :biz/unpaid-total (:def/id %)) (assoc % :def/where [:= :biz/invoice-status :biz.status/paid]) %)
                          (fx/ontology))
            sys2 (assoc sys :ontology (mem/create changed) :cache (atom {}))]
        (is (= #{(:unpaid ids)} (store/dependents s :biz/unpaid-total)) "who depends on the measure, before changing it")
        (is (= #{(:unpaid ids) (:client ids)} (store/dependents s [:rows :biz/client])))
        (is (= {:unpaid true :contract false :client false}
               (into {} (for [[k id] ids] [k (:stale? (lg/plan-status sys2 {:plan-id id}))])))
            "only the dependents of the changed measure are stale")))))

(deftest the-graph-store-survives-a-restart
  (let [path (temp-db)
        s1 (graph/store path)
        sys (ts/system {:store s1})
        {:plan/keys [id]} (lg/compile sys spec-query)]
    (is (nil? (store/set-name! s1 id {:name "unpaid-by-client" :description "Overdue unpaid invoices by client"})))
    (graph/close s1)
    (let [s2 (graph/store path)
          sys2 (ts/system {:store s2})]
      (is (= id (:plan/id (store/get-plan s2 id))))
      (is (= {"unpaid-by-client" {:plan/id id :description "Overdue unpaid invoices by client"}} (store/names s2)))
      (is (= 5 (count (:rows (lg/run sys2 {:plan-id id :user ts/admin :clock ts/clock}))))
          "a fresh process runs the stored plan by id, without the query")
      (graph/close s2))))

(deftest store-contract
  (doseq [[label s] [["memory" (memory/store)] ["graph" (graph/store (temp-db))]]]
    (testing label
      (let [sys (ts/system {:store s})
            a (:plan/id (lg/compile sys spec-query))
            b (:plan/id (lg/compile sys client-query))]
        (is (nil? (store/put-plan! s (store/get-plan s a))) "idempotent")
        (is (= {:error :plan/not-found} (store/set-name! s "sha256:nope" {:name "x"})))
        (store/set-name! s a {:name "n1" :description "d"})
        (store/set-name! s b {:name "n1" :description "moved"})
        (is (= {"n1" {:plan/id b :description "moved"}} (store/names s)) "a name moves to its new plan")
        (store/set-name! s b nil)
        (is (= {} (store/names s)))))))
