(ns loregate.hash
  "Content hashes over canonical EDN: maps and sets are printed in a fixed order, so equal
  values always hash the same, whatever order they were built in."
  (:require [clojure.string :as str])
  (:import (java.security MessageDigest)))

(defn canonical
  "A deterministic EDN string for `x`."
  [x]
  (cond
    (map? x)        (str "{" (str/join "," (sort (map (fn [[k v]] (str (canonical k) " " (canonical v))) x))) "}")
    (set? x)        (str "#{" (str/join "," (sort (map canonical x))) "}")
    (sequential? x) (str "[" (str/join "," (map canonical x)) "]")
    :else           (binding [*print-length* nil *print-level* nil] (pr-str x))))

(defn sha256
  "\"sha256:<hex>\" of the canonical EDN of `x`."
  [x]
  (let [d (.digest (MessageDigest/getInstance "SHA-256") (.getBytes ^String (canonical x) "UTF-8"))]
    (str "sha256:" (apply str (map #(format "%02x" %) d)))))
