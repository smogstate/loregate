(ns loregate.run.duck
  "One in-memory DuckDB connection per run: attach the relations, execute the plan's SQL."
  (:require [clojure.string :as str]
            [loregate.compile.sql :refer [q]])
  (:import (java.sql Connection DriverManager SQLException)
           (org.duckdb DuckDBArray)))

(defn- exec! [^Connection c sql]
  (with-open [st (.createStatement c)] (.execute st sql)))

(defn- attach!
  "Make src_<alias> exist: a view over a scan expression, or a table of supplied rows
  ({:columns [[name list?] …] :rows [[v …] …]}, every value already in storage form)."
  [^Connection c {:keys [alias scan columns rows]}]
  (let [rel (q (str "src_" (name alias)))]
    (if scan
      (exec! c (str "CREATE TEMP VIEW " rel " AS SELECT * FROM " scan))
      (do
        (exec! c (str "CREATE TEMP TABLE " rel " ("
                      (str/join ", " (for [[col list?] columns] (str (q col) (if list? " VARCHAR[]" " VARCHAR"))))
                      ")"))
        (when (seq rows)
          (with-open [ps (.prepareStatement c (str "INSERT INTO " rel " VALUES ("
                                                   (str/join ", " (repeat (count columns) "?")) ")"))]
            (doseq [row rows]
              (doseq [[i [[_ list?] v]] (map-indexed vector (map vector columns row))]
                (.setObject ps (int (inc i))
                            (cond (nil? v) nil
                                  list? (.createArrayOf c "VARCHAR" (object-array (map str v)))
                                  :else (str v))))
              (.executeUpdate ps))))))))

(defn- read-value [v]
  (if (instance? DuckDBArray v) (vec (.getArray ^DuckDBArray v)) v))

(defn error-kind
  "The run error a DuckDB failure stands for, or nil when it is not one we expect."
  [^SQLException e]
  (let [m (str (.getMessage e))]
    (cond (or (str/includes? m "loregate:schema-mismatch") (str/includes? m "Conversion Error")
              (str/includes? m "Binder Error: Referenced column"))
          :source/schema-mismatch
          (str/includes? m "IO Error") :source/unavailable)))

(defn- bind! [^Connection c ps params]
  (doseq [[i v] (map-indexed vector params)]
    (.setObject ^java.sql.PreparedStatement ps (int (inc i))
                (if (coll? v) (.createArrayOf c "VARCHAR" (object-array (map str v))) v))))

(defn evaluate
  "The single row of a parameterised SELECT, as a vector (used for field-rule checks)."
  [sql params]
  (with-open [c (DriverManager/getConnection "jdbc:duckdb:")
              ps (.prepareStatement c sql)]
    (bind! c ps params)
    (with-open [rs (.executeQuery ps)]
      (.next rs)
      (mapv #(.getObject rs (int %)) (range 1 (inc (.getColumnCount (.getMetaData rs))))))))

(defn execute
  "{:rows [[v …] …]} with at most `max-rows` + 1 rows, or {:error kind}. A collection
  parameter is bound as a VARCHAR list (the SQL casts it)."
  [{:keys [relations sql params max-rows memory-limit threads]}]
  (try
    (with-open [c (DriverManager/getConnection "jdbc:duckdb:")]
      (exec! c (str "SET memory_limit='" (str/replace memory-limit "'" "") "'"))   ; our own settings
      (exec! c (str "SET threads=" (long threads)))
      (run! #(attach! c %) relations)
      (with-open [ps (.prepareStatement c sql)]
        (bind! c ps params)
        (with-open [rs (.executeQuery ps)]
          (let [n (.getColumnCount (.getMetaData rs))]
            (loop [rows []]
              (if (and (< (count rows) (inc max-rows)) (.next rs))
                (recur (conj rows (mapv #(read-value (.getObject rs (int %))) (range 1 (inc n)))))
                {:rows rows}))))))
    (catch SQLException e
      (if-let [kind (error-kind e)]
        {:error kind}
        (throw e)))))
