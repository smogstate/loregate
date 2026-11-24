(ns loregate.service-test
  "Phase 4 gate: a query joining a fake service and a Parquet file passes 14.3 and 14.5."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [loregate.core :as lg]
            [loregate.fake-service :as fake]
            [loregate.fixtures :as fx]
            [loregate.run-test :refer [spec-query]]
            [loregate.security-test :as sec]
            [loregate.sources.parquet :as pq]
            [loregate.sources.service :as svc]
            [loregate.test-system :as ts])
  (:import (com.sun.net.httpserver HttpHandler HttpServer)
           (java.net InetSocketAddress)))

(defn- system [calls & {:keys [decls config] :as opts}]
  (ts/system {:decls decls :config config
              :sources {:parquet (pq/adapter ["dev/fixtures/"])
                        :service (svc/adapter (apply fake/transport calls (mapcat identity (dissoc opts :decls :config))))}}))

(def users sec/users)

(defn- run [sys q user & {:as opts}]
  (lg/query sys (merge {:query q :user user :clock ts/clock} opts)))

(deftest binding-selection-and-pushdown
  (let [calls (atom [])
        sys (system calls)
        plan (#'lg/compile* sys spec-query)]
    (testing "the status filter makes the service eligible, and it accepts more filters"
      (is (= :service (get-in plan [:plan/sources :invoice :kind])))
      (is (= [{:arg "status" :op := :value {:value "UNPAID"}}] (get-in plan [:plan/sources :invoice :call :args])))
      (is (= [{:attribute :biz/invoice-status :op :=}] (get-in plan [:plan/sources :invoice :pushed]))))
    (testing "without the required filter, the Parquet binding is chosen"
      (is (= :parquet (get-in (#'lg/compile* sys {:from :biz/invoice :select [:biz/invoice-id]})
                              [:plan/sources :invoice :kind]))))))

(deftest a-service-joined-with-parquet
  (let [calls (atom [])
        sys (system calls)
        res (run sys spec-query (assoc ts/admin :user/credential "the-callers-token"))]
    (is (= (ts/run-query (ts/system) spec-query) (:rows res)) "same rows as over Parquet only")
    (is (< 1 (count @calls)) "pages followed")
    (is (every? #(= {"status" "UNPAID"} (:args %)) @calls) "pushed filter in stored form")
    (is (every? #(= "the-callers-token" (:credential %)) @calls) "called as the user (12.1)")
    (is (inst? (get-in res [:notes :as-of :invoice])))))

(deftest rules-equal-hand-redaction-with-a-service
  ;; 14.3 with the service: service-enforced invoice rows, layer-enforced field rules
  (let [sys (system (atom []))]
    (doseq [u (keys users)
            q [spec-query
               {:from :biz/invoice :select [:biz/invoice-id :biz/amount [:biz.invoice/client :biz/client-name]]
                :where [:= :biz/invoice-status :biz.status/paid]}]]
      (is (= (:rows (run sys q (users u)))
             (:rows (lg/query (sec/open u) {:query q :user {:user/id "nobody" :user/attrs {:principals #{}}}
                                            :clock ts/clock})))
          u))))

(deftest a-pushed-filter-on-a-hidden-field-calls-nothing
  ;; 8.3: status hidden from bob → the service would filter on the true value; it is not called
  (let [calls (atom [])
        decls (conj (fx/ontology) {:rule/on :biz/invoice-status :rule/kind :field
                                   :rule/visible [:in "role:finance" [:user :roles]]})
        sys (system calls :decls decls)]
    (is (= [] (:rows (run sys spec-query (users "bob")))))
    (is (= [] @calls))
    (is (seq (:rows (run sys spec-query (users "ana"))))
        "ana may see status: called")
    (is (seq @calls))))

(deftest source-failures-are-refusals
  (is (= :source/timeout (:error (run (system (atom []) :delay-ms 1000 :config {:source-timeout-ms 100})
                                      spec-query ts/admin))))
  (is (= :source/schema-mismatch (:error (run (system (atom []) :bad-rows? true) spec-query ts/admin))))
  (is (= :source/unavailable (:error (run (system (atom []) :error :source/unavailable) spec-query ts/admin))))
  (testing "past the row cap: an error, never a silent truncation"
    (let [decls (mapv #(if (= :bind/invoices-by-status (:binding/id %)) (assoc % :binding/row-cap 3) %) (fx/ontology))]
      (is (= :source/row-cap-exceeded (:error (run (system (atom []) :decls decls) spec-query ts/admin))))))
  (testing "a query without an output schema cannot be bound"
    (is (contains? (set (map :error (:errors (lg/compile (system (atom []) :no-schema? true) spec-query))))
                   :compile/invalid-binding))))

(deftest the-http-transport
  ;; the shipped transport against a minimal HTTP service
  (let [seen (atom [])
        server (doto (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
                 (.createContext "/" (reify HttpHandler
                                       (handle [_ ex]
                                         (let [path (.getPath (.getRequestURI ex))
                                               body (if (.endsWith path "/schema")
                                                      (json/write-str fake/schema)
                                                      (let [req (json/read-str (slurp (.getRequestBody ex)))]
                                                        (swap! seen conj {:auth (.getFirst (.getRequestHeaders ex) "authorization") :req req})
                                                        (json/write-str {"rows" (filter #(= (get-in req ["args" "status"]) (get % "status")) (fake/rows))
                                                                         "next" nil})))
                                               bs (.getBytes ^String body "UTF-8")]
                                           (.sendResponseHeaders ex 200 (alength bs))
                                           (with-open [os (.getResponseBody ex)] (.write os bs))))))
                 (.start))
        base (str "http://127.0.0.1:" (.getPort (.getAddress server)))]
    (try
      (let [sys (ts/system {:sources {:parquet (pq/adapter ["dev/fixtures/"])
                                      :service (svc/adapter (svc/http-transport {"invoicing" base}))}})
            res (run sys spec-query (assoc ts/admin :user/credential "tok-123"))]
        (is (= (ts/run-query (ts/system) spec-query) (:rows res)))
        (is (= "Bearer tok-123" (:auth (first @seen))))
        (is (= {"status" "UNPAID"} (get-in (first @seen) [:req "args"]))))
      (finally (.stop server 0)))))
