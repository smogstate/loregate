(ns loregate.auth.oidc
  "OIDC JWT identity adapter (SPEC 8.1). Verifies an RS256 bearer token against the
  issuer's published keys (JWKS, cached; refetched once for an unknown key id), checks
  issuer, audience, expiry and not-before, and maps claims to :user/attrs. Refuses rather
  than guesses: any failure is an :auth/… error and nothing runs."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [loregate.auth.port :as port])
  (:import (java.math BigInteger)
           (java.net URI)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)
           (java.security KeyFactory Signature)
           (java.security.spec RSAPublicKeySpec)
           (java.util Base64)))

(defn- b64url ^bytes [^String s] (.decode (Base64/getUrlDecoder) s))
(defn- read-json [s] (json/read-str s))

(defn- http-get-json [url]
  (let [resp (.send (HttpClient/newHttpClient)
                    (.build (HttpRequest/newBuilder (URI/create url)))
                    (HttpResponse$BodyHandlers/ofString))]
    (when (= 200 (.statusCode resp)) (read-json (.body resp)))))

(defn default-fetch-jwks
  "Fetch the issuer's JWKS, discovering jwks_uri from its OpenID configuration unless given."
  [{:keys [issuer jwks-uri]}]
  (let [uri (or jwks-uri (get (http-get-json (str (str/replace issuer #"/$" "") "/.well-known/openid-configuration"))
                              "jwks_uri"))]
    (when uri (http-get-json uri))))

(defn- rsa-key [{:strs [n e]}]
  (.generatePublic (KeyFactory/getInstance "RSA")
                   (RSAPublicKeySpec. (BigInteger. 1 (b64url n)) (BigInteger. 1 (b64url e)))))

(defn- key-for [{:keys [cache fetch-jwks] :as cfg} kid]
  (letfn [(lookup [] (some #(when (and (= kid (get % "kid")) (= "RSA" (get % "kty"))) %) (get @cache "keys")))]
    (or (lookup)
        (do (reset! cache (or (fetch-jwks cfg) {})) (lookup)))))

(defn- verify-rs256 [k ^String signed sig]
  (let [v (doto (Signature/getInstance "SHA256withRSA") (.initVerify (rsa-key k)) (.update (.getBytes signed "US-ASCII")))]
    (.verify v sig)))

(defn- groups-truncated?
  "Some providers drop long group lists and say so (e.g. _claim_names, hasgroups)."
  [claims groups-claim]
  (or (contains? (get claims "_claim_names") groups-claim)
      (true? (get claims "hasgroups"))))

(defn- claim-value [v] (if (sequential? v) (set v) v))

(defn claims->user
  "The user context for verified claims (SPEC 8.1): attrs from the claim map, principals
  computed as user:<sub> plus group:<g>."
  [claims claim-map token]
  (let [sub (get claims "sub")
        attrs (into {} (for [[attr claim] claim-map
                             :let [v (get claims claim)]
                             :when (some? v)]          ; absent stays absent, never defaulted
                         [attr (claim-value v)]))
        groups (get claims (get claim-map :groups "groups"))]
    {:user/id sub
     :user/attrs (assoc attrs :principals (into #{(str "user:" sub)} (map #(str "group:" %)) groups))
     :user/credential token}))

(defn verify
  "Claims of a valid token, or {:error :auth/…}."
  [{:keys [issuer audience leeway-seconds claims] :as cfg} ^String token now-seconds]
  (let [parts (str/split (or token "") #"\.")]
    (if (not= 3 (count parts))
      {:error :auth/invalid-token}
      (try
        (let [[h p s] parts
              header (read-json (String. (b64url h) "UTF-8"))
              payload (read-json (String. (b64url p) "UTF-8"))
              k (when (= "RS256" (get header "alg")) (key-for cfg (get header "kid")))
              aud (get payload "aud")
              leeway (or leeway-seconds 60)]
          (cond
            (nil? k) {:error :auth/invalid-token}
            (not (verify-rs256 k (str h "." p) (b64url s))) {:error :auth/invalid-token}
            (not= issuer (get payload "iss")) {:error :auth/invalid-token}
            (not (if (sequential? aud) (some #{audience} aud) (= audience aud))) {:error :auth/invalid-token}
            (not (and (number? (get payload "exp")) (< now-seconds (+ (get payload "exp") leeway)))) {:error :auth/invalid-token}
            (and (number? (get payload "nbf")) (< (+ now-seconds leeway) (get payload "nbf"))) {:error :auth/invalid-token}
            (nil? (get payload "sub")) {:error :auth/invalid-token}
            (groups-truncated? payload (get claims :groups "groups")) {:error :auth/groups-truncated}
            :else payload))
        (catch Exception _ {:error :auth/invalid-token})))))

(defrecord Oidc [cfg]
  port/Identity
  (authenticate [_ request]
    (let [h (get-in request [:headers "authorization"])
          token (some->> h (re-matches #"(?i)Bearer\s+(\S+)") second)]
      (if-not token
        {:error :auth/missing-token}
        (let [claims (verify cfg token (quot (System/currentTimeMillis) 1000))]
          (if (:error claims)
            claims
            (claims->user claims (:claims cfg) token)))))))

(defn adapter
  "An OIDC identity. `cfg`: :issuer, :audience, :claims {attr claim} (e.g.
  {:roles \"roles\" :region \"region\" :groups \"groups\"}), optionally :jwks-uri,
  :leeway-seconds and :fetch-jwks (fn of cfg → JWKS map; default fetches over HTTPS)."
  [cfg]
  (->Oidc (merge {:fetch-jwks default-fetch-jwks} cfg {:cache (atom {})})))
