(ns loregate.schema-test
  "Phase 0 gate: every SPEC example validates; generated queries validate; the grammar
  refuses what it should."
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.error :as me]
            [malli.generator :as mg]
            [loregate.schema.expr :as expr]
            [loregate.schema.query :as query]
            [loregate.schema.ontology :as ont]
            [loregate.schema.plan :as plan]
            [loregate.schema.result :as result]))

(defn- valid? [schema x]
  (or (m/validate schema x)
      (do (println "INVALID:" (pr-str x) "\n  " (me/humanize (m/explain schema x))) false)))

;; ------------------------------------------------------------ SPEC examples

(def spec-query                                              ; SPEC 6
  {:from   :biz/invoice
   :select [[:biz.invoice/client :biz/client-name] :biz/unpaid-total :n]
   :let    {:n [:count]}
   :where  [:and [:= :biz/invoice-status :biz.status/unpaid]
                 [:> :biz/days-overdue [:param :min-days]]]
   :having [:> :biz/unpaid-total 0]
   :sort   [[:biz/unpaid-total :desc]]
   :limit  50
   :params {:min-days {:type :int :default 30 :min 0}}})

(def spec-declarations                                       ; SPEC 5.1 – 5.5
  [{:type/id :biz/invoice :type/title "Invoice" :type/owner "team:finance"
    :type/definition "A request for payment issued to a client."
    :type/key [:biz/invoice-id]
    :type/attributes {:biz/invoice-id {:type :string}
                      :biz/client-id {:type :string}
                      :biz/amount {:type :decimal :unit :currency/eur}
                      :biz/due-date {:type :date}
                      :biz/invoice-status {:type :enum :values #{:biz.status/paid :biz.status/unpaid}}}}
   {:link/id :biz.invoice/client :link/from :biz/invoice :link/to :biz/client
    :link/on [[:biz/client-id :biz/client-id]] :link/cardinality :many-to-one}
   {:def/id :biz/days-overdue :def/on :biz/invoice :def/kind :attribute
    :def/owner "team:finance" :def/type :int
    :def/expr [:days-between :biz/due-date [:today]]}
   {:def/id :biz/unpaid-total :def/on :biz/invoice :def/kind :measure
    :def/owner "team:finance" :def/type :decimal :def/unit :currency/eur
    :def/expr [:sum :biz/amount]
    :def/where [:= :biz/invoice-status :biz.status/unpaid]}
   {:binding/id :bind/invoices-by-status :binding/type :biz/invoice :binding/kind :service
    :service/name "invoicing" :service/query "invoices_by_status"
    :binding/fields {:biz/invoice-id "id" :biz/client-id "client_id" :biz/amount "amount"
                     :biz/due-date "due_date" :biz/invoice-status "status"}
    :binding/values {:biz/invoice-status {:biz.status/paid "PAID" :biz.status/unpaid "UNPAID"}}
    :binding/accepts {:biz/invoice-status {:arg "status" :ops #{:=}}}
    :binding/required #{:biz/invoice-status}
    :binding/row-cap 200000
    :binding/rows-enforced-by :service}
   {:binding/id :bind/clients-parquet :binding/type :biz/client :binding/kind :parquet
    :parquet/path "s3://lake/crm/clients/*.parquet"
    :binding/fields {:biz/client-id "id" :biz/client-name "name" :biz/region "region"}
    :binding/rows-enforced-by :layer}
   {:rule/on :biz/client :rule/kind :rows
    :rule/allow [:or [:in "role:crm-admin" [:user :roles]] [:= :biz/region [:user :region]]]}
   {:rule/on :biz/amount :rule/kind :field
    :rule/visible [:in "role:finance" [:user :roles]]}
   {:rule/on :biz/contract :rule/kind :rows
    :rule/allow [:overlaps :biz/contract-readers [:user :principals]]}
   {:rule/on :biz/contract-value :rule/kind :field
    :rule/visible [:or [:in "role:legal" [:user :roles]]
                       [:overlaps :biz/contract-value-readers [:user :principals]]]}
   {:command/id :biz.invoice/cancel :command/on :biz/invoice
    :command/params [:map [:reason :string]]
    :command/service "invoicing" :command/endpoint "cancel_invoice"}])

(defn- h [c] (str "sha256:" (apply str (repeat 64 c))))

(def spec-plan                                               ; SPEC 7.1
  {:plan/id (h "9")
   :plan/query spec-query
   :plan/sources
   {:inv {:kind :service :type :biz/invoice :path [] :binding :bind/invoices-by-status
          :call {:service "invoicing" :query "invoices_by_status"
                 :args [{:arg "status" :op := :value {:value "UNPAID"}}]}
          :pushed [{:attribute :biz/invoice-status :op :=}]
          :fields {:biz/client-id "client_id" :biz/amount "amount" :biz/due-date "due_date"
                   :biz/invoice-status "status"}
          :row-cap 200000}
    :cl {:kind :parquet :type :biz/client :path [:biz.invoice/client] :binding :bind/clients-parquet
         :parquet/path "s3://lake/crm/clients/*.parquet"
         :fields {:biz/client-id "id" :biz/client-name "name" :biz/region "region"}}}
   :plan/sql "WITH inv AS (…) SELECT …"
   :plan/sql-params [{:value "UNPAID"} {:param :min-days} {:clock :today} {:user :roles}
                     {:visible [:field :biz/amount]}]
   :plan/params {:min-days {:type :int :default 30 :min 0}}
   :plan/user #{:roles :region}
   :plan/requires {:fields #{:biz/amount :biz/client-name :biz/client-id :biz/due-date
                             :biz/invoice-status :biz/region}
                   :rules #{[:rows :biz/client] [:field :biz/amount]}}
   :plan/output [{:name :biz/client-name :type :string :owned? true}
                 {:name :biz/unpaid-total :type :decimal :unit :currency/eur :owned? true}
                 {:name :n :type :int :owned? false}]
   :plan/pins {:biz/invoice (h "a") :biz/client (h "b") :biz.invoice/client (h "c")
               :biz/unpaid-total (h "d") :biz/days-overdue (h "e")
               :bind/invoices-by-status (h "f") :bind/clients-parquet (h "0")
               [:rows :biz/client] (h "1") [:field :biz/amount] (h "2")}
   :plan/rules {:rows #{:biz/client} :fields {:biz/amount :user}}
   :plan/checks {:sql "SELECT CAST(list_contains(CAST(? AS VARCHAR[]), CAST(? AS VARCHAR)) AS BOOLEAN)"
                 :params [{:user :roles} {:value "role:finance"}]
                 :rules [[:field :biz/amount]]}
   :plan/compiler 1
   :plan/compiled-at #inst "2026-11-24T20:00:00Z"})

(def spec-result                                             ; SPEC 9
  {:rows [{:biz/client-name "ACME" :biz/unpaid-total 120000.00M :n 14}]
   :columns [{:name :biz/client-name :type :string :owned? true}]
   :notes {:plan (h "9")
           :as-of {:inv #inst "2026-11-24T20:00:01Z" :cl #inst "2026-11-24T20:00:00Z"}
           :clock {:now #inst "2026-11-24T20:00:00Z" :today "2026-11-24" :zone "UTC"}
           :hidden #{:biz/amount}
           :partial #{}
           :rows-filtered #{:biz/client}}})

(deftest spec-examples-validate
  (is (valid? query/Query spec-query))
  (doseq [d spec-declarations] (is (valid? ont/Declaration d)))
  (is (valid? plan/Plan spec-plan))
  (is (valid? result/Result spec-result))
  (is (valid? plan/CompileError {:error :compile/unknown-name :at [:select 1] :name :biz/foo}))
  (is (valid? result/RunError {:error :plan/stale :changed [:biz/unpaid-total]})))

;; ------------------------------------------------------------ generated

(deftest generated-values-validate
  (doseq [[schema n] [[query/Query 200] [expr/Expr 300] [expr/RuleExpr 300] [expr/DefExpr 300]]
          x (mg/sample schema {:size 12 :seed 42 :count n})]
    (is (m/validate schema x) (pr-str x))))

;; ------------------------------------------------------------ refusals

(deftest grammar-refuses
  (testing "operators are closed and arities fixed"
    (is (not (m/validate expr/Expr [:foo 1 2])))
    (is (not (m/validate expr/Expr [:= 1])))
    (is (not (m/validate expr/Expr [:count 1 2])))
    (is (m/validate expr/Expr [:count]))
    (is (m/validate expr/Expr [:count :biz/x]))
    (is (m/validate expr/Expr [:date-trunc :month :biz/due-date]))
    (is (not (m/validate expr/Expr [:date-trunc :hour :biz/due-date])))
    (is (not (m/validate expr/Expr '(:= 1 1))) "a list is not an expression"))
  (testing "[:user k] only in rules, [:param k] only in queries"
    (is (not (m/validate expr/Expr [:in "role:x" [:user :roles]])))
    (is (m/validate expr/RuleExpr [:in "role:x" [:user :roles]]))
    (is (not (m/validate expr/RuleExpr [:= :biz/a [:param :p]])))
    (is (not (m/validate expr/DefExpr [:= :biz/a [:param :p]]))))
  (testing "collection literals are sets"
    (is (m/validate expr/Expr [:in :biz/status #{:biz.status/paid :biz.status/unpaid}]))
    (is (= ::expr/path (expr/kind [:biz.status/paid :biz.status/unpaid])) "a vector is a path"))
  (testing "locals may not use reserved words"
    (is (not (m/validate query/Query {:from :biz/invoice :select [:and]})))
    (is (m/validate query/Query {:from :biz/invoice :select [:n] :let {:n [:count]}})))
  (testing "queries are closed maps"
    (is (not (m/validate query/Query {:from :biz/invoice :select [:biz/amount] :sql "x"}))))
  (testing "bindings may accept only operators false on an empty value (SPEC 8.3)"
    (is (not (m/validate ont/Binding
                         {:binding/id :b/x :binding/type :t/x :binding/kind :service
                          :binding/fields {:t/a "a"}
                          :binding/accepts {:t/a {:arg "a" :ops #{:is-null}}}
                          :binding/rows-enforced-by :service}))))
  (testing "a field is a column, or a column plus a JSON path (SPEC 5.3)"
    (let [b {:binding/id :b/x :binding/type :t/x :binding/kind :parquet
             :binding/rows-enforced-by :layer}]
      (is (m/validate ont/Binding (assoc b :binding/fields {:t/id "id"
                                                            :t/region ["attrs" "$.address.region"]
                                                            :t/tags ["attrs" "$.tags[*]"]})))
      (is (not (m/validate ont/Binding (assoc b :binding/fields {:t/region ["attrs" "address.region"]}))))))
  (testing "errors are closed sets"
    (is (not (m/validate plan/CompileError {:error :compile/oops})))
    (is (not (m/validate result/RunError {:error :source/oops})))))
