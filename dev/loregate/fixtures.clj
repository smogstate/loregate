(ns loregate.fixtures
  "Fixture loading for tests, and the generator for dev/fixtures/data/*.parquet."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.sql DriverManager)))

(def dir "dev/fixtures")

(defn- read-edn [file] (edn/read-string (slurp (io/file dir file))))

(defn ontology [] (read-edn "ontology.edn"))
(defn users    [] (read-edn "users.edn"))
(defn data     [] (read-edn "data.edn"))
(defn parquet-path
  ([table] (str dir "/data/" (name table) ".parquet"))
  ([user table] (str dir "/data/redacted/" user "/" (name table) ".parquet")))

(defn redacted-users
  "Users with a hand-redacted copy of data.edn in dev/fixtures/redacted/."
  []
  (sort (keep #(second (re-matches #"(.+)\.edn" (.getName %))) (.listFiles (io/file dir "redacted")))))

(defn redacted [user] (read-edn (str "redacted/" user ".edn")))

(defn duckdb
  "A fresh in-memory DuckDB connection. Close it."
  []
  (DriverManager/getConnection "jdbc:duckdb:"))

(defn- placeholder [type]
  (if (str/ends-with? type "[]")
    "list_filter(string_split(?, ','), x -> x <> '')"
    (str "CAST(? AS " type ")")))

(defn write-parquet!
  "Write one table of data.edn (or of a redacted copy) to `path`."
  [conn table {:keys [columns rows]} path]
  (with-open [st (.createStatement conn)]
    (.execute st (format "CREATE OR REPLACE TABLE %s (%s)" (name table)
                         (str/join ", " (for [[c t] columns] (str c " " t))))))
  (with-open [ps (.prepareStatement conn (format "INSERT INTO %s VALUES (%s)" (name table)
                                                 (str/join ", " (map (comp placeholder second) columns))))]
    (doseq [row rows]
      (doseq [[i v] (map-indexed vector row)]
        (.setString ps (inc i) (if (coll? v) (str/join "," v) v)))   ; nil → NULL
      (.executeUpdate ps)))
  (io/make-parents path)
  (with-open [st (.createStatement conn)]
    (.execute st (format "COPY %s TO '%s' (FORMAT parquet)" (name table) path))))

(defn -main [& _]
  (with-open [conn (duckdb)]
    (doseq [[table spec] (data)]
      (write-parquet! conn table spec (parquet-path table))
      (println "wrote" (parquet-path table) (count (:rows spec)) "rows"))
    (doseq [user (redacted-users)
            [table spec] (redacted user)]
      (write-parquet! conn table spec (parquet-path user table))
      (println "wrote" (parquet-path user table) (count (:rows spec)) "rows"))))
