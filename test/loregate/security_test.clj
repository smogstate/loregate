(ns loregate.security-test
  "The gate that matters (SPEC 14.3): for generated queries and every fixture user, running
  with rules over the real data equals running with NO rules over a copy of the data that
  was redacted by hand for that user (dev/fixtures/redacted/<user>.edn)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [loregate.core :as lg]
            [loregate.fixtures :as fx]
            [loregate.gen :as gen]
            [loregate.run-test :refer [spec-query]]
            [loregate.test-system :as ts]))

(def users (into {} (map (juxt :user/id identity)) (fx/users)))

(defn- open-ontology
  "The fixture ontology with every rule removed, each type opened deliberately, and the
  Parquet bindings pointed at `user`'s redacted files."
  [user]
  (let [decls (remove :rule/kind (fx/ontology))]
    (into (vec (for [d decls]
                 (if-let [path (:parquet/path d)]
                   (assoc d :parquet/path (fx/parquet-path user (re-find #"[a-z]+(?=\.parquet$)" path)))
                   d)))
          (for [d decls :when (:type/id d)]
            {:rule/on (:type/id d) :rule/kind :rows :rule/allow true}))))

(def ^:private real (ts/system))
(def ^:private open (into {} (for [u (fx/redacted-users)] [u (ts/system {:decls (open-ontology u)})])))

(defn- rows [sys q user]
  (let [res (lg/query sys {:query q :user user :clock ts/clock})]
    (or (:rows res) res)))

(defn- equivalent? [q user-id]
  (= (rows real q (users user-id))
     (rows (open user-id) q {:user/id "nobody" :user/attrs {:principals #{}}})))

(deftest the-gate-has-teeth
  ;; If rules did nothing, or applied the wrong user's rights, the comparison must fail.
  (doseq [[a b] [["ana" "bob"] ["bob" "cleo"] ["cleo" "ana"]]
          q [{:from :biz/client :select [:biz/client-id]}
             {:from :biz/contract :select [:biz/contract-id :biz/contract-value]}]]
    (is (not= (rows real q (users a))
              (rows (open b) q {:user/id "nobody" :user/attrs {:principals #{}}}))
        (str a " vs " b " " (pr-str q))))
  (is (not= (rows real {:from :biz/invoice :select [:biz/amount]} (users "bob"))
            (rows (ts/system) {:from :biz/invoice :select [:biz/amount]} ts/admin))
      "bob sees no amounts"))

(deftest every-fixture-user-has-a-hand-redacted-copy
  (is (= (sort (keys users)) (fx/redacted-users))))

(deftest rules-equal-hand-redaction
  (testing "fixture queries"
    (doseq [u (keys users)
            q [spec-query
               {:from :biz/client :select [:biz/client-name :biz/region :biz/client-tags]}
               {:from :biz/invoice :select [[:biz.invoice/client :biz/region] :total :n]
                :let {:total [:sum :biz/amount] :n [:count]}}
               {:from :biz/invoice :select [:biz/invoice-id] :where [:> :biz/amount 500]}
               {:from :biz/contract :select [:biz/contract-id :biz/contract-value :biz/contract-readers
                                             [:biz.contract/client :biz/client-name]]}
               {:from :biz/contract :select [:biz/contract-total]}]]
      (is (equivalent? q u) (str u " " (pr-str q)))))
  (testing "generated queries"
    (let [result (tc/quick-check 150 (prop/for-all [q gen/query, u (clojure.test.check.generators/elements (keys users))]
                                       (equivalent? q u))
                                 :seed 11)]
      (is (:pass? result) (pr-str (get-in result [:shrunk :smallest]))))))

(deftest notes-explain-what-was-cut
  (let [notes (fn [u q] (:notes (lg/query real {:query q :user (users u) :clock ts/clock})))]
    (is (= #{:biz/amount} (:hidden (notes "bob" spec-query))))
    (is (= #{} (:hidden (notes "ana" spec-query))))
    (is (= #{:biz/client} (:rows-filtered (notes "ana" spec-query))) "a row rule applied, even if it cut nothing")
    (is (= #{:biz/contract-value} (:partial (notes "bob" {:from :biz/contract :select [:biz/contract-value]}))))))

(deftest fail-closed
  (testing "a layer-enforced type with no row rule cannot be queried"
    (let [decls (remove #(= [:rows :biz/client] [(:rule/kind %) (:rule/on %)]) (fx/ontology))]
      (is (= #{:compile/no-row-rule}
             (set (map :error (:errors (lg/compile (ts/system {:decls decls})
                                                   {:from :biz/client :select [:biz/client-id]}))))))))
  (testing "a user attribute the token lacks makes a rule false, never true"
    (is (= [] (rows real {:from :biz/client :select [:biz/client-id]} {:user/id "x" :user/attrs {:principals #{}}})))
    (is (= #{:biz/amount}
           (:hidden (:notes (lg/query real {:query {:from :biz/invoice :select [:biz/amount]}
                                            :user {:user/id "x" :user/attrs {:principals #{}}}
                                            :clock ts/clock}))))))
  (testing "a plan is the same for every user"
    (is (= (:plan/id (lg/compile real spec-query)) (:plan/id (lg/compile (ts/system) spec-query))))))
