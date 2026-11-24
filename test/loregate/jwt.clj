(ns loregate.jwt
  "Test tokens: an RSA key pair, its JWKS, and RS256-signed JWTs."
  (:require [clojure.data.json :as json])
  (:import (java.security KeyPairGenerator Signature)
           (java.security.interfaces RSAPublicKey)
           (java.util Base64)))

(defonce key-pair (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA") (.initialize 2048))))
(defonce other-pair (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA") (.initialize 2048))))

(defn- b64 [^bytes bs] (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bs))
(defn- unsigned [^BigInteger n] (let [bs (.toByteArray n)] (if (zero? (first bs)) (java.util.Arrays/copyOfRange bs 1 (alength bs)) bs)))

(def issuer "https://idp.test/")
(def audience "loregate")

(def jwks
  (let [^RSAPublicKey pub (.getPublic key-pair)]
    {"keys" [{"kty" "RSA" "kid" "k1" "alg" "RS256" "use" "sig"
              "n" (b64 (unsigned (.getModulus pub))) "e" (b64 (unsigned (.getPublicExponent pub)))}]}))

(def oidc-config
  {:issuer issuer :audience audience
   :claims {:roles "roles" :region "region" :groups "groups"}
   :fetch-jwks (fn [_] jwks)})

(defn token
  "A signed JWT for `claims` (merged over a valid default), signed with `pair`."
  ([claims] (token claims key-pair))
  ([claims pair]
   (let [now (quot (System/currentTimeMillis) 1000)
         header (b64 (.getBytes (json/write-str {"alg" "RS256" "kid" "k1" "typ" "JWT"}) "UTF-8"))
         payload (b64 (.getBytes (json/write-str (merge {"iss" issuer "aud" audience "sub" "someone"
                                                         "exp" (+ now 300) "iat" now}
                                                        claims))
                                 "UTF-8"))
         signer (doto (Signature/getInstance "SHA256withRSA") (.initSign (.getPrivate pair))
                  (.update (.getBytes (str header "." payload) "US-ASCII")))]
     (str header "." payload "." (b64 (.sign signer))))))

(def ana-claims {"sub" "ana" "roles" ["role:finance" "role:crm-admin"] "region" "EU" "groups" ["finance"]})
(def bob-claims {"sub" "bob" "roles" [] "region" "US" "groups" ["legal-us"]})
