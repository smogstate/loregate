(ns loregate.named-test
  "Phase 6 gate: a named plan returns the same rows as running it by id."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [loregate.api.http :as http]
            [loregate.auth.oidc :as oidc]
            [loregate.jwt :as jwt]
            [loregate.run-test :refer [spec-query]]
            [loregate.store.memory :as memory]
            [loregate.test-system :as ts]))

(defn- handler [store role]
  (http/handler (ts/system {:store store :config {:publish/role role}}) (oidc/adapter jwt/oidc-config)))

(defn- call [h method path token & [body]]
  (let [{:keys [status body]} (h {:method method :path path :query-string nil
                                  :headers {"authorization" (str "Bearer " token) "content-type" "application/edn"
                                            "accept" "application/edn"}
                                  :body (pr-str (or body {}))})]
    [status (edn/read-string body)]))

(def ana (jwt/token jwt/ana-claims))     ; roles finance, crm-admin
(def bob (jwt/token jwt/bob-claims))     ; no roles

(deftest a-named-plan-runs-like-its-id
  (let [h (handler (memory/store) "role:crm-admin")
        [_ {id :plan/id}] (call h "POST" "/api/v1/plans" ana spec-query)
        [st named] (call h "PUT" (str "/api/v1/plans/" id "/name") ana {:name "unpaid-by-client" :description "Overdue unpaid by client"})]
    (is (= [200 {:name "unpaid-by-client" :plan/id id}] [st named]))
    (let [by-id (call h "POST" (str "/api/v1/plans/" id "/run") ana {:params {:min-days 60}})
          by-name (call h "POST" "/api/v1/named/unpaid-by-client/run" ana {:params {:min-days 60}})]
      (is (= 200 (first by-id) (first by-name)))
      (is (seq (:rows (second by-id))))
      (is (= (select-keys (second by-id) [:rows :columns]) (select-keys (second by-name) [:rows :columns])))
      (is (= id (get-in by-name [1 :notes :plan]))))
    (testing "rules still apply per caller"
      (is (= #{:biz/amount} (get-in (second (call h "POST" "/api/v1/named/unpaid-by-client/run" bob {})) [:notes :hidden]))))
    (testing "describe lists named plans with their contract"
      (let [[_ d] (call h "GET" "/api/v1/ontology" bob)]
        (is (= [{:name "unpaid-by-client" :description "Overdue unpaid by client" :plan/id id}]
               (map #(select-keys % [:name :description :plan/id]) (:named d))))
        (is (= [:biz/client-name :biz/unpaid-total :n] (map :name (:plan/output (first (:named d))))))))
    (testing "only the publish role may name or unname"
      (is (= [403 {:error :auth/forbidden}] (call h "DELETE" (str "/api/v1/plans/" id "/name") bob))))
    (testing "unpublish"
      (is (= 200 (first (call h "DELETE" (str "/api/v1/plans/" id "/name") ana))))
      (is (= [404 {:error :plan/not-found}] (call h "POST" "/api/v1/named/unpaid-by-client/run" ana {}))))))

(deftest refusals
  (let [h (handler (memory/store) "role:crm-admin")
        [_ {id :plan/id}] (call h "POST" "/api/v1/plans" ana spec-query)]
    (is (= 400 (first (call h "PUT" (str "/api/v1/plans/" id "/name") ana {:name "has spaces"}))))
    (is (= [404 {:error :plan/not-found}] (call h "PUT" "/api/v1/plans/sha256:nope/name" ana {:name "x"}))))
  (testing "no publish role configured: nobody publishes"
    (let [h (handler (memory/store) nil)
          [_ {id :plan/id}] (call h "POST" "/api/v1/plans" ana spec-query)]
      (is (= [403 {:error :auth/forbidden}] (call h "PUT" (str "/api/v1/plans/" id "/name") ana {:name "x"})))))
  (testing "a store that keeps no names"
    (let [h (http/handler (ts/system {:config {:publish/role "role:crm-admin"}}) (oidc/adapter jwt/oidc-config))
          [_ {id :plan/id}] (call h "POST" "/api/v1/plans" ana spec-query)]
      (is (= [501 {:error :store/not-supported}] (call h "PUT" (str "/api/v1/plans/" id "/name") ana {:name "x"}))))))
