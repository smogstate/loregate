(ns loregate.run-test
  "Phase 1 gate: the fixture queries return the expected rows (worked out by hand from
  dev/fixtures/data.edn, today = 2026-11-24)."
  (:require [clojure.test :refer [deftest is testing]]
            [loregate.core :as lg]
            [loregate.fixtures :as fx]
            [loregate.test-system :as ts :refer [run-query]])
  (:import (java.time LocalDate)))

(def spec-query
  {:from   :biz/invoice
   :select [[:biz.invoice/client :biz/client-name] :biz/unpaid-total :n]
   :let    {:n [:count]}
   :where  [:and [:= :biz/invoice-status :biz.status/unpaid]
                 [:> :biz/days-overdue [:param :min-days]]]
   :having [:> :biz/unpaid-total 0]
   :sort   [[:biz/unpaid-total :desc]]
   :limit  50
   :params {:min-days {:type :int :default 30 :min 0}}})

(deftest spec-example
  (is (= [{:biz/client-name "Umbrella" :biz/unpaid-total 7200.00M :n 1}
          {:biz/client-name "ACME"     :biz/unpaid-total 4200.00M :n 2}
          {:biz/client-name nil        :biz/unpaid-total 999.00M  :n 1}   ; c9: no such client
          {:biz/client-name "Initech"  :biz/unpaid-total 600.00M  :n 2}
          {:biz/client-name "Globex"   :biz/unpaid-total 450.00M  :n 1}]
         (run-query (ts/system) spec-query)))
  (testing "a param changes the result, a default does not have to be sent"
    (is (= ["Umbrella" "ACME" nil "Globex"]                  ; i12 (c9) is 108 days overdue
           (map :biz/client-name (run-query (ts/system) spec-query :params {:min-days 100}))))))

(deftest fixture-queries
  (let [sys (ts/system)]
    (testing "plain select, sorted"
      (is (= [["ACME" "EU"] ["Globex" "US"] ["Hooli" "US"] ["Initech" "US"] ["Umbrella" "EU"]]
             (map (juxt :biz/client-name :biz/region)
                  (run-query sys {:from :biz/client :select [:biz/client-name :biz/region]
                                  :sort [[:biz/client-name :asc]]})))))
    (testing "attributes inside a JSON column (SPEC 5.3)"
      (is (= [["ACME" "enterprise" #{"key" "eu"}]
              ["Globex" "enterprise" #{"us"}]
              ["Hooli" "smb" #{}]                ; no tags key: an empty set
              ["Initech" "smb" #{}]
              ["Umbrella" nil #{"key"}]]         ; no segment key: empty
             (map (juxt :biz/client-name :biz/client-segment :biz/client-tags)
                  (run-query sys {:from :biz/client
                                  :select [:biz/client-name :biz/client-segment :biz/client-tags]
                                  :sort [[:biz/client-name :asc]]})))))
    (testing "grouping on a path, local aggregates, empty group last"
      (is (= [{:biz/region "EU" :total 12300.50M :n 5}
              {:biz/region "US" :total 4190.99M :n 6}
              {:biz/region nil :total 999.00M :n 1}]
             (run-query sys {:from :biz/invoice
                             :select [[:biz.invoice/client :biz/region] :total :n]
                             :let {:total [:sum :biz/amount] :n [:count]}
                             :sort [[[:biz.invoice/client :biz/region] :asc]]}))))
    (testing "set literal, between, a date param with a default"
      (is (= [{:biz/invoice-id "i02" :biz/amount 800.50M} {:biz/invoice-id "i10" :biz/amount 100.00M}]
             (run-query sys {:from :biz/invoice
                             :select [:biz/invoice-id :biz/amount]
                             :where [:and [:in :biz/invoice-status #{:biz.status/paid}]
                                          [:between :biz/due-date [:param :from] "2026-12-31"]]
                             :params {:from {:type :date :default "2026-07-01"}}
                             :sort [[:biz/invoice-id :asc]]}))))
    (testing "a derived attribute and date-trunc"
      (is (= [{:biz/invoice-id "i07" :biz/days-overdue 84 :month (LocalDate/parse "2026-09-01")}
              {:biz/invoice-id "i08" :biz/days-overdue 84 :month (LocalDate/parse "2026-09-01")}]
             (run-query sys {:from :biz/invoice
                             :select [:biz/invoice-id :biz/days-overdue :month]
                             :let {:month [:date-trunc :month :biz/due-date]}
                             :where [:= :biz/client-id "c3"]
                             :sort [[:biz/invoice-id :asc]]}))))
    (testing "sets: overlaps with a literal"
      (is (= ["k2" "k3"]
             (map :biz/contract-id
                  (run-query sys {:from :biz/contract :select [:biz/contract-id]
                                  :where [:overlaps :biz/contract-readers #{"group:legal-us"}]
                                  :sort [[:biz/contract-id :asc]]})))))
    (testing "a measure with no grouping key: one row"
      (is (= [{:biz/unpaid-total 15089.99M}]
             (run-query sys {:from :biz/invoice :select [:biz/unpaid-total]}))))
    (testing "enum values come back as keywords"
      (is (= #{:biz.status/paid :biz.status/unpaid}
             (set (map :biz/invoice-status
                       (run-query sys {:from :biz/invoice :select [:biz/invoice-status]}))))))))

(deftest implicit-limit-is-reported
  (let [res (lg/query (ts/system {:config {:max-rows 3}})
                      {:query {:from :biz/client :select [:biz/client-id] :sort [[:biz/client-id :asc]]}
                       :user ts/ana :clock ts/clock})]
    (is (= ["c1" "c2" "c3"] (map :biz/client-id (:rows res))))
    (is (true? (get-in res [:notes :truncated]))))
  (testing "an explicit :limit is not truncation"
    (let [res (lg/query (ts/system {:config {:max-rows 3}})
                        {:query {:from :biz/client :select [:biz/client-id] :limit 4}
                         :user ts/ana :clock ts/clock})]
      (is (= 4 (count (:rows res))))
      (is (nil? (get-in res [:notes :truncated]))))))

(deftest notes
  (let [res (lg/query (ts/system) {:query spec-query :user ts/ana :clock ts/clock})]
    (is (= {:now #inst "2026-11-24T12:00:00Z" :today "2026-11-24" :zone "UTC"} (get-in res [:notes :clock])))
    (is (= #{:invoice :client} (set (keys (get-in res [:notes :as-of])))))
    (is (= (get-in res [:notes :plan]) (:plan/id (lg/compile (ts/system) spec-query))))))

(deftest supplied-relations-give-the-same-rows
  (let [sys (ts/system)
        {:plan/keys [id]} (lg/compile sys spec-query)
        relations {:invoice (ts/ontology-rows :bind/invoices-parquet)
                   :client (ts/ontology-rows :bind/clients-parquet)}]
    (is (= (run-query sys spec-query)
           (:rows (lg/run sys {:plan-id id :user ts/ana :clock ts/clock :relations relations}))))
    (testing "a different clock gives a different, deterministic answer"
      (is (= ["Umbrella" "ACME" nil "Globex"]
             (map :biz/client-name
                  (:rows (lg/run sys {:plan-id id :user ts/ana :relations relations
                                      :clock {:now #inst "2026-09-10T00:00:00Z" :zone "UTC"}}))))))
    (testing "a value the binding cannot store is a schema mismatch"
      (is (= :source/schema-mismatch
             (:error (lg/run sys {:plan-id id :user ts/ana :clock ts/clock
                                  :relations (assoc relations :invoice [{:biz/invoice-status :biz.status/void}])}))))
      (is (= :source/schema-mismatch
             (:error (lg/run sys {:plan-id id :user ts/ana :clock ts/clock
                                  :relations (assoc relations :invoice [{:biz/invoice-status :biz.status/unpaid
                                                                         :biz/amount "not a number"
                                                                         :biz/due-date "2026-01-01"}])})))))))

(deftest params-are-checked
  (let [sys (ts/system)]
    (is (= :params/invalid (:error (lg/query sys {:query spec-query :params {:min-days -1} :user ts/ana}))))
    (is (= :params/invalid (:error (lg/query sys {:query spec-query :params {:nope 1} :user ts/ana}))))
    (is (= :params/invalid (:error (lg/query sys {:query spec-query :params {:min-days "x"} :user ts/ana}))))))

(deftest run-by-id
  (let [sys (ts/system)
        {:plan/keys [id]} (lg/compile sys spec-query)]
    (is (= 5 (count (:rows (lg/run sys {:plan-id id :user ts/ana :clock ts/clock})))) "held in the cache")
    (testing "with no store, a fresh system needs the query and recompiles it"
      (is (= :plan/not-found (:error (lg/run (ts/system) {:plan-id id :user ts/ana}))))
      (is (= 5 (count (:rows (lg/run (ts/system) {:plan-id id :query spec-query :user ts/ana :clock ts/clock}))))))
    (testing "no user, nothing runs"
      (is (= :auth/missing-token (:error (lg/run sys {:plan-id id})))))))

(deftest missing-file-is-unavailable
  (let [decls (mapv #(if (= :bind/clients-parquet (:binding/id %))
                       (assoc % :parquet/path "dev/fixtures/data/missing.parquet") %)
                    (fx/ontology))]
    (is (= {:error :source/unavailable}
           (run-query (ts/system {:decls decls}) {:from :biz/client :select [:biz/client-id]})))))
