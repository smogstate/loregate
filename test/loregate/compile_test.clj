(ns loregate.compile-test
  "Determinism, golden plans, staleness, and one case per compile error (SPEC 14.2)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [loregate.compile.core :as c]
            [loregate.core :as lg]
            [loregate.fixtures :as fx]
            [loregate.ontology.memory :as mem]
            [loregate.run-test :refer [spec-query]]
            [loregate.schema.plan :as plan]
            [loregate.sources.parquet :as pq]
            [loregate.store.memory :as store-mem]
            [loregate.store.port :as store]
            [loregate.test-system :as ts]))

(def sources {:parquet (pq/adapter ["dev/fixtures/"])})

(defn- compile* [q & {:keys [decls srcs]}]
  (c/compile-query (mem/create (or decls (fx/ontology))) q {:sources (or srcs sources)}))

(defn- error-of [q & opts]
  (let [res (apply compile* q opts)]
    (set (map :error (:errors res)))))

(deftest plans-validate-and-are-deterministic
  (let [p1 (compile* spec-query)
        p2 (compile* spec-query)
        p3 (compile* spec-query :decls (vec (shuffle (fx/ontology))))]
    (is (m/validate plan/Plan p1) (pr-str (m/explain plan/Plan p1)))
    (is (= (:plan/id p1) (:plan/id p2) (:plan/id p3)))
    (is (= (dissoc p1 :plan/compiled-at) (dissoc p3 :plan/compiled-at)))
    (testing "the id is the hash of the plan without :plan/compiled-at"
      (is (not= (:plan/compiled-at p1) (:plan/compiled-at (do (Thread/sleep 2) (compile* spec-query)))))
      (is (= (:plan/id p1) (:plan/id (compile* spec-query)))))
    (testing "a plan holds no user value and pins what it used"
      (is (= #{} (:plan/user p1)))
      (is (= #{:biz/invoice :biz/client :biz.invoice/client :biz/days-overdue :biz/unpaid-total
               :bind/invoices-parquet :bind/clients-parquet}
             (set (keys (:plan/pins p1))))))))

(def golden-queries
  {:spec-example spec-query
   :json-fields {:from :biz/client :select [:biz/client-name :biz/client-tags]
                 :where [:= :biz/client-segment "smb"]}
   :sets {:from :biz/contract :select [:biz/contract-id]
          :where [:subset #{"group:legal-us"} :biz/contract-readers]}})

(deftest golden-plans
  ;; A missing golden file is written and the test passes: review it, then commit it.
  (doseq [[k q] golden-queries]
    (let [f (io/file "test/golden" (str (name k) ".edn"))
          p (compile* q)
          got (select-keys p [:plan/id :plan/sql :plan/sql-params :plan/sources :plan/output])]
      (if (.exists f)
        (is (= (edn/read-string (slurp f)) got) (str "golden plan changed: " f))
        (do (io/make-parents f)
            (spit f (with-out-str (pprint got)))
            (println "wrote" (str f)))))))

(deftest every-compile-error-has-a-case
  (let [q {:from :biz/invoice :select [:biz/invoice-id]}
        with-client-measure (conj (fx/ontology)
                                  {:def/id :biz/client-count :def/on :biz/client :def/kind :measure
                                   :def/owner "team:crm" :def/type :int :def/expr [:count]})
        cases [[:compile/invalid-query       (error-of {:from :biz/invoice})]
               [:compile/unknown-name        (error-of (assoc q :select [:biz/nope]))]
               [:compile/unknown-name        (error-of (assoc q :from :biz/nope))]
               [:compile/bad-path            (error-of (assoc q :select [[:biz.contract/client :biz/client-name]]))]
               [:compile/type-mismatch       (error-of (assoc q :where [:> :biz/client-id 3]))]
               [:compile/type-mismatch       (error-of (assoc q :where [:= :biz/invoice-status :biz/amount]))]
               [:compile/aggregate-in-where  (error-of (assoc q :where [:> :biz/unpaid-total 0]))]
               [:compile/measure-not-on-from (error-of (assoc q :select [[:biz.invoice/client :biz/client-count]])
                                                       :decls with-client-measure)]
               [:compile/duplicate-column    (error-of (assoc q :select [:biz/client-id [:biz.invoice/client :biz/client-id]]))]
               [:compile/sort-not-selected   (error-of (assoc q :sort [[:biz/amount :asc]]))]
               [:compile/cycle               (error-of (assoc q :select [:a] :let {:a :b :b :a}))]
               [:compile/unknown-param       (error-of (assoc q :where [:> :biz/amount [:param :x]]))]
               [:compile/limit-too-large     (error-of (assoc q :limit 20000))]
               [:compile/no-binding          (error-of q :srcs {})]
               [:compile/invalid-binding     (error-of q :srcs {:parquet (pq/adapter ["s3://elsewhere/"])})]]]
    (doseq [[k errs] cases]
      (is (contains? errs k) (str k " expected, got " errs)))
    (testing "every documented error is covered; :compile/no-row-rule comes with rules (phase 2)"
      (is (= (disj plan/compile-errors :compile/no-row-rule) (set (map first cases)))))))

(deftest stale-plans-do-not-run
  (let [sys (ts/system)
        {:plan/keys [id]} (lg/compile sys spec-query)
        changed (mapv #(if (= :biz/unpaid-total (:def/id %)) (assoc % :def/where [:= :biz/invoice-status :biz.status/paid]) %)
                      (fx/ontology))
        sys2 (assoc sys :ontology (mem/create changed))]
    (is (false? (:stale? (lg/plan-status sys {:plan-id id}))))
    (is (= {:stale? true :changed [:biz/unpaid-total]}
           (select-keys (lg/plan-status sys2 {:plan-id id}) [:stale? :changed])))
    (is (= {:error :plan/stale :changed [:biz/unpaid-total]}
           (lg/run sys2 {:plan-id id :user ts/ana :clock ts/clock})))
    (testing "a plan from another compiler version is stale"
      (swap! (:cache sys) update id assoc :plan/compiler 0)
      (is (= [:plan/compiler] (:changed (lg/plan-status sys {:plan-id id})))))
    (testing "the memory store answers what depends on a declaration"
      (let [store (store-mem/store)
            sys (ts/system {:store store})
            {:plan/keys [id]} (lg/compile sys spec-query)]
        (is (= #{id} (store/dependents store :biz/unpaid-total)))
        (is (= #{} (store/dependents store :biz/contract-total)))))))
