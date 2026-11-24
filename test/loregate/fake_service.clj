(ns loregate.fake-service
  "A fake CQRS query service over the fixture invoices (storage names, as a service would
  return them). Pages its results, records every call, and can be made slow, broken or
  unavailable."
  (:require [loregate.fixtures :as fx]
            [loregate.sources.service :as svc]))

(def schema
  {"type" "object"
   "required" ["id" "client_id" "status"]
   "properties" {"id" {"type" "string"} "client_id" {"type" "string"}
                 "amount" {"type" ["number" "null"]} "due_date" {"type" "string"}
                 "status" {"type" "string"}}})

(defn rows []
  (let [{:keys [columns rows]} (:invoices (fx/data))]
    (for [r rows]
      (-> (zipmap (map first columns) r)
          (update "amount" #(some-> % bigdec))))))

(defn transport
  "A Transport. Options: :page (rows per page, default 5), :delay-ms, :bad-rows?,
  :error (returned instead of rows), :no-schema?. `calls` (an atom) records
  {:args :credential :cursor} per call."
  [calls & {:keys [page delay-ms bad-rows? error no-schema?] :or {page 5}}]
  (reify svc/Transport
    (output-schema [_ service query]
      (when (and (= ["invoicing" "invoices_by_status"] [service query]) (not no-schema?)) schema))
    (call [_ _ _ args credential cursor]
      (swap! calls conj {:args args :credential credential :cursor cursor})
      (when delay-ms (Thread/sleep (long delay-ms)))
      (if error
        {:error error}
        (let [all (cond->> (rows)
                    (get args "status") (filter #(= (get args "status") (get % "status")))
                    bad-rows? (map #(assoc % "amount" "not a number")))
              offset (or cursor 0)
              chunk (take page (drop offset all))
              next (+ offset page)]
          {:rows (vec chunk) :next (when (< next (count all)) next)})))))
