(ns loregate.gen
  "Generators of queries that are valid against the fixture ontology (not just the
  grammar): real names, well-typed filters, optional measures and grouping."
  (:require [clojure.test.check.generators :as g]))

(def ^:private shapes
  {:biz/invoice
   {:columns  [:biz/invoice-id :biz/client-id :biz/amount :biz/due-date :biz/invoice-status
               :biz/days-overdue [:biz.invoice/client :biz/client-name]
               [:biz.invoice/client :biz/region] [:biz.invoice/client :biz/client-segment]]
    :measures [:biz/unpaid-total]
    :filters  [[:= :biz/invoice-status :biz.status/unpaid]
               [:in :biz/client-id #{"c1" "c2" "c9"}]
               [:> :biz/amount 500]
               [:<= :biz/due-date "2026-09-30"]
               [:> :biz/days-overdue [:param :d]]
               [:= [:biz.invoice/client :biz/region] "EU"]
               [:is-null [:biz.invoice/client :biz/client-name]]]}
   :biz/client
   {:columns  [:biz/client-id :biz/client-name :biz/region :biz/client-segment :biz/client-tags]
    :measures []
    :filters  [[:= :biz/region "US"]
               [:starts-with :biz/client-name "I"]
               [:overlaps :biz/client-tags #{"key"}]
               [:not [:= :biz/client-segment "smb"]]]}
   :biz/contract
   {:columns  [:biz/contract-id :biz/client-id :biz/contract-value :biz/contract-readers
               [:biz.contract/client :biz/client-name] [:biz.contract/client :biz/region]]
    :measures [:biz/contract-total]
    :filters  [[:overlaps :biz/contract-readers #{"group:legal-us" "user:ana"}]
               [:> :biz/contract-value 5000]
               [:= [:biz.contract/client :biz/region] "EU"]]}})

(defn- subset [xs min max]
  (g/fmap #(vec (distinct %)) (g/vector (g/elements xs) min max)))

(defn- query-from [from]
  (let [{:keys [columns measures filters]} (get shapes from)]
    (g/let [cols     (subset columns 1 3)
            measure? (if (seq measures) g/boolean (g/return false))
            wheres   (subset filters 0 2)
            limit    (g/one-of [(g/return nil) (g/choose 1 5)])
            desc?    g/boolean]
      (let [select (cond-> cols measure? (conj (first measures)))
            where (case (count wheres) 0 nil 1 (first wheres) (into [:and] wheres))]
        (cond-> {:from from :select select
                 :sort [[(first select) (if desc? :desc :asc)]]}
          where (assoc :where where)
          (some #{[:param :d]} (tree-seq vector? seq where)) (assoc :params {:d {:type :int :default 60}})
          limit (assoc :limit limit))))))

(def query (g/bind (g/elements (keys shapes)) query-from))
