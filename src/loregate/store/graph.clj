(ns loregate.store.graph
  "Graph plan store (SPEC 11): a plan is a node (id, body) with an edge to every
  declaration it pins (type, link, definition, binding, rule), carrying the pinned hash.
  \"What depends on this measure?\" walks incoming edges. The edge says depends-on; the pin
  on it says on exactly this version.

  This adapter keeps the graph in a DuckDB file: durable across restarts on one node. A
  Loretile-backed adapter implements the same port over Loretile's graph."
  (:require [clojure.edn :as edn]
            [loregate.store.port :as port])
  (:import (java.sql Connection DriverManager)))

(defn- exec! [^Connection c sql & params]
  (with-open [ps (.prepareStatement c sql)]
    (doseq [[i p] (map-indexed vector params)] (.setObject ps (int (inc i)) p))
    (.execute ps)))

(defn- query [^Connection c sql & params]
  (with-open [ps (.prepareStatement c sql)]
    (doseq [[i p] (map-indexed vector params)] (.setObject ps (int (inc i)) p))
    (with-open [rs (.executeQuery ps)]
      (let [n (.getColumnCount (.getMetaData rs))]
        (loop [out []]
          (if (.next rs) (recur (conj out (mapv #(.getObject rs (int %)) (range 1 (inc n))))) out))))))

(defn- decl-key [id] (pr-str id))            ; :biz/unpaid-total, [:rows :biz/client]

(defrecord Graph [^Connection conn]
  port/PlanStore
  (put-plan! [_ plan]
    (locking conn
      (when (empty? (query conn "SELECT 1 FROM plan_nodes WHERE id = ?" (:plan/id plan)))
        (exec! conn "INSERT INTO plan_nodes VALUES (?, ?)" (:plan/id plan) (pr-str plan))
        (doseq [[decl pin] (sort-by (comp str key) (:plan/pins plan))]
          (exec! conn "INSERT INTO depends_on VALUES (?, ?, ?)" (:plan/id plan) (decl-key decl) pin))))
    nil)

  (get-plan [_ id]
    (locking conn
      (some-> (ffirst (query conn "SELECT body FROM plan_nodes WHERE id = ?" id)) edn/read-string)))

  (dependents [_ declaration-id]
    (locking conn
      (into #{} (map first) (query conn "SELECT plan_id FROM depends_on WHERE decl = ?" (decl-key declaration-id)))))

  (set-name! [_ id named]
    (locking conn
      (if (empty? (query conn "SELECT 1 FROM plan_nodes WHERE id = ?" id))
        {:error :plan/not-found}
        (do (exec! conn "DELETE FROM plan_names WHERE plan_id = ?" id)
            (when named
              (exec! conn "DELETE FROM plan_names WHERE name = ?" (:name named))
              (exec! conn "INSERT INTO plan_names VALUES (?, ?, ?)" (:name named) id (:description named)))
            nil))))

  (names [_]
    (locking conn
      (into {} (for [[n id d] (query conn "SELECT name, plan_id, description FROM plan_names")]
                 [n {:plan/id id :description d}])))))

(defn store
  "A graph store in the DuckDB file at `path` (created if missing)."
  [path]
  (let [c (DriverManager/getConnection (str "jdbc:duckdb:" path))]
    (exec! c "CREATE TABLE IF NOT EXISTS plan_nodes (id VARCHAR PRIMARY KEY, body VARCHAR NOT NULL)")
    (exec! c "CREATE TABLE IF NOT EXISTS depends_on (plan_id VARCHAR NOT NULL, decl VARCHAR NOT NULL, pin VARCHAR NOT NULL)")
    (exec! c "CREATE TABLE IF NOT EXISTS plan_names (name VARCHAR PRIMARY KEY, plan_id VARCHAR NOT NULL, description VARCHAR)")
    (->Graph c)))

(defn close [^Graph s] (.close ^Connection (:conn s)))
