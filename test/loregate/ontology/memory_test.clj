(ns loregate.ontology.memory-test
  (:require [clojure.test :refer [deftest is testing]]
            [loregate.fixtures :as fx]
            [loregate.ontology.memory :as mem]
            [loregate.ontology.port :as o]))

(def onto (mem/create (fx/ontology)))

(defn- problem-kinds [decls]
  (set (map :problem (mem/problems decls))))

(deftest fixture-ontology-loads
  (is (nil? (mem/problems (fx/ontology))))
  (is (= [:biz/client :biz/contract :biz/invoice] (map :type/id (o/types onto)))))

(deftest type-of-gathers-everything-about-a-type
  (let [t (o/type-of onto :biz/invoice)]
    (is (= [:biz.invoice/client] (map :link/id (:type/links t))))
    (is (= #{:biz/days-overdue :biz/unpaid-total} (set (map :def/id (:type/defs t)))))
    (is (= #{:bind/invoices-by-status :bind/invoices-parquet} (set (map :binding/id (:type/bindings t)))))
    (is (= #{[:rows :biz/invoice] [:field :biz/amount]}
           (set (map (juxt :rule/kind :rule/on) (:type/rules t)))))
    (is (= [:biz.invoice/cancel] (map :command/id (:type/commands t)))))
  (testing "a field rule follows its attribute to every type that has it"
    (is (some #(= :biz/contract-value (:rule/on %)) (:type/rules (o/type-of onto :biz/contract)))))
  (is (nil? (o/type-of onto :biz/nope))))

(deftest search-finds-types-attributes-and-measures
  (is (= [{:kind :measure :id :biz/unpaid-total :on :biz/invoice}] (o/search onto "UNPAID-tot")))
  (is (some #{{:kind :type :id :biz/invoice}} (o/search onto "request for payment")))
  (is (some #{{:kind :attribute :id :biz/region :on :biz/client}} (o/search onto "region"))))

(deftest hash-of-pins-one-declaration
  (let [decls (fx/ontology)
        changed (mapv #(if (= :biz/unpaid-total (:def/id %)) (assoc % :def/expr [:sum :biz/amount :biz/amount]) %)
                      decls)
        changed (mapv #(if (= :biz/unpaid-total (:def/id %)) (assoc % :def/where [:= :biz/invoice-status :biz.status/paid]) %)
                      decls)
        onto2 (mem/create changed)]
    (is (re-matches #"sha256:[0-9a-f]{64}" (o/hash-of onto :biz/invoice)))
    (is (= (o/hash-of onto :biz/invoice) (o/hash-of (mem/create (reverse decls)) :biz/invoice)) "order-free")
    (is (not= (o/hash-of onto :biz/unpaid-total) (o/hash-of onto2 :biz/unpaid-total)) "a change shows")
    (is (= (o/hash-of onto :biz/days-overdue) (o/hash-of onto2 :biz/days-overdue)) "and only there")
    (is (some? (o/hash-of onto [:rows :biz/client])))
    (is (nil? (o/hash-of onto :biz/nope))))
  (testing "a source adapter's schema is part of a binding's hash"
    (let [a (mem/create (fx/ontology) {:schema-hash (constantly "v1")})
          b (mem/create (fx/ontology) {:schema-hash (constantly "v2")})]
      (is (not= (o/hash-of a :bind/clients-parquet) (o/hash-of b :bind/clients-parquet)))
      (is (= (o/hash-of a :biz/client) (o/hash-of b :biz/client))))))

(deftest invalid-ontologies-are-refused
  (let [base (fx/ontology)
        with (fn [& ds] (into base ds))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid ontology"
                          (mem/create (with {:link/id :x/y}))))
    (is (contains? (problem-kinds (with {:type/id :biz/client :type/title "Again" :type/owner "x"
                                         :type/key [:biz/client-id]
                                         :type/attributes {:biz/client-id {:type :string}}}))
                   :duplicate-id))
    (is (contains? (problem-kinds (with {:type/id :biz/other :type/title "Other" :type/owner "x"
                                         :type/key [:biz/client-id]
                                         :type/attributes {:biz/client-id {:type :int}}}))
                   :attribute-conflict))
    (is (contains? (problem-kinds (with {:link/id :biz.invoice/ghost :link/from :biz/invoice
                                         :link/to :biz/ghost :link/on [[:biz/client-id :biz/x]]
                                         :link/cardinality :many-to-one}))
                   :unknown-type))
    (is (contains? (problem-kinds (with {:rule/on :biz/ghost-field :rule/kind :field :rule/visible true}))
                   :unknown-rule-target))
    (is (contains? (problem-kinds (with {:binding/id :bind/bad :binding/type :biz/invoice
                                         :binding/kind :parquet
                                         :binding/fields {:biz/invoice-status "status"}
                                         :binding/rows-enforced-by :layer}))
                   :enum-without-values))
    (is (contains? (problem-kinds (with {:binding/id :bind/bad2 :binding/type :biz/client
                                         :binding/kind :service
                                         :binding/fields {:biz/client-id "id"}
                                         :binding/required #{:biz/client-id}
                                         :binding/rows-enforced-by :service}))
                   :required-not-accepted))))
