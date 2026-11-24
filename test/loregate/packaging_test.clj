(ns loregate.packaging-test
  "Phase 7 gate: a second deployment with its own ontology file, identity provider and
  Parquet location runs the fixture queries without a code change, and every shipped
  adapter passes its conformance kit."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [loregate.auth.oidc :as oidc]
            [loregate.compile.core :as c]
            [loregate.config :as config]
            [loregate.conformance :as kit]
            [loregate.core :as lg]
            [loregate.fake-service :as fake]
            [loregate.fixtures :as fx]
            [loregate.jwt :as jwt]
            [loregate.main :as main]
            [loregate.ontology.memory :as mem]
            [loregate.run-test :refer [spec-query]]
            [loregate.schema.json :as sj]
            [loregate.security-test :as sec]
            [loregate.sources.parquet :as pq]
            [loregate.sources.service :as svc]
            [loregate.store.graph :as graph]
            [loregate.store.memory :as memory]
            [loregate.store.none :as none]
            [loregate.test-system :as ts])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.security KeyPairGenerator Signature)
           (java.util Base64)))

;; ------------------------------------------------------------ settings

(deftest settings-file-then-environment
  (let [s (config/resolve-settings {:http {:port 9000} :store {:adapter :memory}}
                                   {"LOREGATE_HTTP_PORT" "9100"
                                    "LOREGATE_STORAGE_ALLOWED" "[\"s3://lake/\"]"
                                    "LOREGATE_IDENTITY_ISSUER" "https://idp.example/"
                                    "LOREGATE_PUBLISH_ROLE" "role:publisher"
                                    "LOREGATE_NOT_A_SETTING" "ignored"})]
    (is (= 9100 (get-in s [:http :port])) "environment over file")
    (is (= :memory (get-in s [:store :adapter])) "file over defaults")
    (is (= ["s3://lake/"] (:storage/allowed s)))
    (is (= "https://idp.example/" (get-in s [:identity :issuer])) "non-EDN text is a string")
    (is (= "role:publisher" (:publish/role s)))
    (is (= 10000 (:max-rows s)) "defaults remain")
    (is (= "LOREGATE_DUCKDB_MEMORY_LIMIT" (config/env-name [:duckdb :memory-limit])))))

;; ------------------------------------------------------------ a second deployment

(defn- copy-tree [from to]
  (doseq [^java.io.File f (file-seq (io/file from)) :when (.isFile f)]
    (let [target (io/file to (subs (.getPath f) (inc (count from))))]
      (io/make-parents target)
      (io/copy f target))))

(deftest a-second-deployment-without-code-changes
  ;; its own directory, ontology file, Parquet location and identity provider (other keys,
  ;; other issuer, other claim names)
  (let [dir (str (Files/createTempDirectory "loregate-second" (make-array FileAttribute 0)))
        _ (copy-tree "dev/fixtures/data" (str dir "/lake"))
        ontology (str/replace (slurp "dev/fixtures/ontology.edn") "dev/fixtures/data/" (str dir "/lake/"))
        _ (spit (str dir "/ontology.edn") ontology)
        pair (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA") (.initialize 2048)))
        b64 #(.encodeToString (.withoutPadding (Base64/getUrlEncoder)) %)
        unsigned #(let [bs (.toByteArray ^BigInteger %)] (if (zero? (first bs)) (java.util.Arrays/copyOfRange bs 1 (alength bs)) bs))
        jwks {"keys" [{"kty" "RSA" "kid" "k1" "n" (b64 (unsigned (.getModulus (.getPublic pair))))
                       "e" (b64 (unsigned (.getPublicExponent (.getPublic pair))))}]}
        jwks-file (doto (io/file dir "jwks.json") (spit (json/write-str jwks)))
        settings-file (doto (io/file dir "settings.edn")
                        (spit (pr-str {:ontology {:adapter :edn :path (str dir "/ontology.edn")}
                                       :store {:adapter :graph :path (str dir "/plans.db")}
                                       :storage/allowed [(str dir "/lake/")]
                                       :identity {:adapter :oidc :issuer "https://second-idp/" :audience "reports"
                                                  :claims {:roles "app_roles" :region "geo" :groups "teams"}}})))
        settings (assoc-in (config/load-settings (str settings-file)) [:identity :fetch-jwks] (fn [_] jwks))
        token (let [now (quot (System/currentTimeMillis) 1000)
                    enc #(b64 (.getBytes (json/write-str %) "UTF-8"))
                    h (enc {"alg" "RS256" "kid" "k1"})
                    p (enc {"iss" "https://second-idp/" "aud" "reports" "sub" "zoe" "exp" (+ now 300)
                            "app_roles" ["role:finance" "role:crm-admin"] "geo" "EU" "teams" ["finance"]})
                    sig (doto (Signature/getInstance "SHA256withRSA") (.initSign (.getPrivate pair)) (.update (.getBytes (str h "." p) "US-ASCII")))]
                (str h "." p "." (b64 (.sign sig))))
        handler (main/handler settings)
        call (fn [method path body]
               (let [{:keys [status body]} (handler {:method method :path path :query-string nil
                                                     :headers {"authorization" (str "Bearer " token)
                                                               "content-type" "application/edn" "accept" "application/edn"}
                                                     :body (pr-str body)})]
                 [status (edn/read-string body)]))
        [st compiled] (call "POST" "/api/v1/plans" spec-query)
        [st2 res] (call "POST" (str "/api/v1/plans/" (:plan/id compiled) "/run") {:params {:min-days 30}})]
    (is (.exists jwks-file))
    (is (= 200 st st2))
    (is (= ["Umbrella" "ACME" nil "Initech" "Globex"] (map :biz/client-name (:rows res))))
    (is (= "7200.00" (:biz/unpaid-total (first (:rows res)))))
    (is (.exists (io/file dir "plans.db")) "plans kept in the second deployment's own graph store")
    (testing "the first deployment's identity provider is not trusted here"
      (is (= 401 (:status (handler {:method "GET" :path "/api/v1/ontology"
                                    :headers {"authorization" (str "Bearer " (jwt/token jwt/ana-claims))}})))))))

;; ------------------------------------------------------------ conformance kits

(deftest shipped-adapters-pass-their-kits
  (let [onto (mem/create (fx/ontology))
        plan (c/compile-query onto spec-query {:sources (:sources (ts/system))})
        other (c/compile-query onto {:from :biz/client :select [:biz/client-name]} {:sources (:sources (ts/system))})
        binding-of (fn [id] (some #(when (= id (:binding/id %)) %) (fx/ontology)))]
    (is (= [] (kit/ontology-kit onto)) "memory ontology")
    (doseq [[label make] [["none" none/store] ["memory" memory/store]
                          ["graph" #(graph/store (str (Files/createTempFile "kit" ".db" (make-array FileAttribute 0)) ".graph"))]]]
      (is (= [] (kit/store-kit make plan other)) (str label " store")))
    (is (= [] (kit/source-kit (pq/adapter ["dev/fixtures/"]) (binding-of :bind/clients-parquet)
                              (get-in other [:plan/sources :client]) ts/admin))
        "parquet source")
    (let [svc-plan (c/compile-query onto spec-query {:sources {:parquet (pq/adapter ["dev/fixtures/"])
                                                               :service (svc/adapter (fake/transport (atom [])))}})]
      (is (= [] (kit/source-kit (svc/adapter (fake/transport (atom []))) (binding-of :bind/invoices-by-status)
                                (get-in svc-plan [:plan/sources :invoice]) ts/admin))
          "service source"))
    (is (= [] (kit/identity-kit (oidc/adapter jwt/oidc-config)
                                {:headers {"authorization" (str "Bearer " (jwt/token jwt/ana-claims))}}
                                [{:headers {"authorization" "Bearer x.y.z"}}
                                 {:headers {"authorization" (str "Bearer " (jwt/token jwt/ana-claims jwt/other-pair))}}]))
        "oidc identity")
    (is (= [] (kit/redaction-kit {:queries [spec-query {:from :biz/contract :select [:biz/contract-id :biz/contract-value]}]
                                  :users (keys sec/users)
                                  :run-with-rules (fn [q u] (:rows (lg/query sec/real {:query q :user (sec/users u) :clock ts/clock})))
                                  :run-open (fn [q u] (:rows (lg/query (sec/open u) {:query q :user {:user/id "x" :user/attrs {:principals #{}}}
                                                                                     :clock ts/clock})))}))
        "parquet through the redaction kit")))

(deftest kits-catch-a-broken-adapter
  (let [broken (reify loregate.store.port/PlanStore
                 (put-plan! [_ _] nil) (get-plan [_ _] nil) (dependents [_ _] #{})
                 (set-name! [_ _ _] nil) (names [_] {"x" {:plan/id "?"}}))
        onto (mem/create (fx/ontology))
        plan (c/compile-query onto spec-query {:sources (:sources (ts/system))})]
    (is (seq (kit/store-kit (constantly broken) plan plan)))))

;; ------------------------------------------------------------ JSON Schema

(deftest json-schema-export
  (let [out (sj/export)]
    (is (= #{"query" "expression" "result" "run-error" "compile-error" "column" "inputs"} (set (keys out))))
    (is (every? map? (vals out)))
    (is (string? (json/write-str out)) "serialisable")))
