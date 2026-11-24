(ns loregate.http-test
  "Phase 3 gate: a client goes from describe to rows with a token and nothing else."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [loregate.api.http :as http]
            [loregate.auth.oidc :as oidc]
            [loregate.jwt :as jwt]
            [loregate.test-system :as ts])
  (:import (java.net URI)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)))

(def ^:dynamic *base* nil)

(use-fixtures :once
  (fn [t]
    (let [server (http/start (http/handler (ts/system) (oidc/adapter jwt/oidc-config)) {:port 0 :host "127.0.0.1"})]
      (try (binding [*base* (str "http://127.0.0.1:" (.getPort (.getAddress server)))] (t))
           (finally (.stop server 0))))))

(defn- call
  "[status body] for a request; `fmt` :edn or :json selects both directions."
  [method path {:keys [token body fmt] :or {fmt :edn}}]
  (let [ct (if (= fmt :json) "application/json" "application/edn")
        b (.uri (HttpRequest/newBuilder) (URI/create (str *base* path)))
        b (cond-> (.header b "accept" ct)
            token (.header "authorization" (str "Bearer " token))
            body (.header "content-type" ct))
        b (.method b method (if body
                              (HttpRequest$BodyPublishers/ofString (if (= fmt :json) (json/write-str body) (pr-str body)))
                              (HttpRequest$BodyPublishers/noBody)))
        resp (.send (HttpClient/newHttpClient) (.build b) (HttpResponse$BodyHandlers/ofString))]
    [(.statusCode resp) (if (= fmt :json) (json/read-str (.body resp)) (edn/read-string (.body resp)))]))

(def ana (jwt/token jwt/ana-claims))
(def bob (jwt/token jwt/bob-claims))

(deftest no-token-no-data
  (is (= [401 {:error :auth/missing-token}] (call "GET" "/api/v1/ontology" {}))))

(deftest bad-tokens-are-refused
  (let [now (quot (System/currentTimeMillis) 1000)]
    (doseq [[why t err] [["signed by another key" (jwt/token jwt/ana-claims jwt/other-pair) :auth/invalid-token]
                         ["expired" (jwt/token (assoc jwt/ana-claims "exp" (- now 3600))) :auth/invalid-token]
                         ["other audience" (jwt/token (assoc jwt/ana-claims "aud" "someone-else")) :auth/invalid-token]
                         ["other issuer" (jwt/token (assoc jwt/ana-claims "iss" "https://evil/")) :auth/invalid-token]
                         ["not yet valid" (jwt/token (assoc jwt/ana-claims "nbf" (+ now 3600))) :auth/invalid-token]
                         ["garbage" "abc.def" :auth/invalid-token]
                         ["truncated groups" (jwt/token (assoc jwt/ana-claims "_claim_names" {"groups" "src1"}))
                          :auth/groups-truncated]]]
      (is (= [401 {:error err}] (call "GET" "/api/v1/ontology" {:token t})) why))))

(deftest describe-to-rows-with-a-token-only
  (testing "describe: types, attributes with visibility for this user, no storage"
    (let [[st body] (call "GET" "/api/v1/ontology?types=biz/invoice,biz/contract" {:token bob})
          attrs (into {} (for [t (:types body), a (:attributes t)] [(:id a) (:visible? a)]))]
      (is (= 200 st))
      (is (= [:biz/invoice :biz/contract] (map :type/id (:types body))))
      (is (= false (attrs :biz/amount)) "bob is not in finance")
      (is (= :per-row (attrs :biz/contract-value)))
      (is (= true (attrs :biz/due-date)))
      (is (not (re-find #"parquet|src_|SELECT" (pr-str body))) "no storage in describe")))
  (testing "search"
    (let [[_ body] (call "GET" "/api/v1/ontology?search=unpaid" {:token ana})]
      (is (some #{{:kind :measure :id :biz/unpaid-total :on :biz/invoice}} (:matches body)))))
  (testing "compile, then run by id: EDN"
    (let [q {:from :biz/invoice :select [:biz/invoice-id :biz/amount :biz/due-date :biz/invoice-status]
             :where [:= :biz/client-id "c3"] :sort [[:biz/invoice-id :asc]]}
          [st compiled] (call "POST" "/api/v1/plans" {:token ana :body q})
          [st2 res] (call "POST" (str "/api/v1/plans/" (:plan/id compiled) "/run") {:token ana :body {}})]
      (is (= 200 st st2))
      (is (= [{:biz/invoice-id "i07" :biz/amount "300.00" :biz/due-date "2026-09-01" :biz/invoice-status :biz.status/unpaid}
              {:biz/invoice-id "i08" :biz/amount "300.00" :biz/due-date "2026-09-01" :biz/invoice-status :biz.status/unpaid}]
             (:rows res))
          "decimals and dates are strings; enums keywords (SPEC 10)")
      (is (string? (get-in res [:notes :clock :now])))
      (testing "the same plan for another user: rules applied for them"
        (let [[_ res] (call "POST" (str "/api/v1/plans/" (:plan/id compiled) "/run") {:token bob :body {}})]
          (is (= [nil nil] (map :biz/amount (:rows res))))
          (is (= #{:biz/amount} (get-in res [:notes :hidden])))))
      (testing "plan-status"
        (let [[st s] (call "GET" (str "/api/v1/plans/" (:plan/id compiled)) {:token bob})]
          (is (= 200 st))
          (is (false? (:stale? s)))))))
  (testing "the same over JSON"
    (let [q {"from" "biz/invoice"
             "select" [["biz.invoice/client" "biz/client-name"] "biz/unpaid-total" "n"]
             "let" {"n" ["count"]}
             "where" ["and" ["=" "biz/invoice-status" "biz.status/unpaid"]
                      [">" "biz/days-overdue" ["param" "min-days"]]
                      ["in" "biz/client-id" {"$set" ["c1" "c4"]}]]
             "sort" [["biz/unpaid-total" "desc"]]
             "params" {"min-days" {"type" "int" "default" 30 "min" 0}}}
          [st res] (call "POST" "/api/v1/query" {:token ana :fmt :json :body {"query" q "params" {"min-days" 60}}})]
      (is (= 200 st))
      (is (= ["Umbrella" "ACME"] (map #(get % "biz/client-name") (get res "rows"))))
      (is (every? string? (map #(get % "biz/unpaid-total") (get res "rows"))) "decimals stay exact"))))

(deftest errors-are-data
  (is (= 400 (first (call "POST" "/api/v1/plans" {:token ana :body {:from :biz/invoice :select [:biz/nope]}}))))
  (is (= :compile/unknown-name
         (-> (call "POST" "/api/v1/plans" {:token ana :body {:from :biz/invoice :select [:biz/nope]}}) second :errors first :error)))
  (is (= [404 {:error :plan/not-found}]
         (call "POST" "/api/v1/plans/sha256:0000/run" {:token ana :body {}})))
  (is (= 400 (first (call "POST" "/api/v1/query" {:token ana :body {:query {:from :biz/invoice :select [:biz/amount]}
                                                                      :params {:x 1}}}))))
  (is (= [400 {:error :request/invalid}]
         (let [[st b] (call "POST" "/api/v1/nothing" {:token ana :body {}})] [st (select-keys b [:error])]))))
