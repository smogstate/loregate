(ns loregate.sources.parquet
  "Parquet source adapter (SPEC 12.2): a scan source read by DuckDB inside the statement."
  (:require [clojure.string :as str]
            [loregate.sources.port :as port]))

(defn- sql-string [s] (str "'" (str/replace s "'" "''") "'"))

(defrecord Parquet [allowed]                  ; allowed: storage location prefixes (SPEC 8.4)
  port/Source
  (kind [_] :parquet)

  (check [_ binding]
    (let [path (:parquet/path binding)]
      (seq (cond-> []
             (not (string? path))
             (conj {:problem :missing-path})
             (and (string? path) (not-any? #(str/starts-with? path %) allowed))
             (conj {:problem :path-not-allowed})
             (not= :layer (:binding/rows-enforced-by binding))
             (conj {:problem :rows-must-be-enforced-by-layer})))))

  ;; Pins cover the declared fields and types only; drift is caught at run (SPEC 12.2).
  (schema-hash [_ _] nil)

  ;; The path comes from a binding only, never from a request.
  (relation [_ source]
    {:scan (str "read_parquet(" (sql-string (:parquet/path source)) ")")})

  (fetch [_ _ _ _]
    {:error :source/unavailable}))

(defn adapter
  "A Parquet adapter that reads only under the given location prefixes."
  [allowed]
  (->Parquet (vec allowed)))
