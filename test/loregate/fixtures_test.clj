(ns loregate.fixtures-test
  "The fixtures agree with each other: every Parquet binding's file exists, has the
  mapped columns and the rows of data.edn; every enum value stored is mapped."
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [loregate.auth.port :as auth]
            [loregate.fixtures :as fx]))

(defn- query [conn sql]
  (with-open [st (.createStatement conn)
              rs (.executeQuery st sql)]
    (let [md (.getMetaData rs) n (.getColumnCount md)]
      (loop [rows []]
        (if (.next rs)
          (recur (conj rows (into {} (for [i (range 1 (inc n))] [(.getColumnName md i) (.getObject rs i)]))))
          rows)))))

(deftest parquet-matches-bindings
  (with-open [conn (fx/duckdb)]
    (doseq [{:binding/keys [id fields values] :as b} (fx/ontology)
            :when (= :parquet (:binding/kind b))]
      (testing (str id)
        (let [path (:parquet/path b)
              table (keyword (re-find #"[a-z]+(?=\.parquet$)" path))
              rows (query conn (format "SELECT * FROM read_parquet('%s')" path))]
          (is (= (count (get-in (fx/data) [table :rows])) (count rows)))
          (is (= (set (map #(if (vector? %) (first %) %) (vals fields))) (set (keys (first rows))))
              "columns are exactly the mapped fields")
          (doseq [[attr mapping] values]
            (is (every? (set (vals mapping)) (map #(get % (fields attr)) rows))
                (str attr " has an unmapped stored value"))))))))

(deftest fixture-users-are-user-contexts
  (is (<= 2 (count (fx/users))))
  (doseq [u (fx/users)]
    (is (m/validate auth/User u) (:user/id u))
    (is (contains? (get-in u [:user/attrs :principals]) (str "user:" (:user/id u))))))
