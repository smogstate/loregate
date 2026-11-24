(ns loregate.store.memory
  "Plans, names and dependency edges in memory, until restart (SPEC 11)."
  (:require [loregate.store.port :as port]))

(defrecord Memory [state]                     ; atom {:plans {id plan} :names {name {:plan/id :description}}}
  port/PlanStore
  (put-plan! [_ plan]
    (swap! state assoc-in [:plans (:plan/id plan)] plan)
    nil)
  (get-plan [_ id]
    (get-in @state [:plans id]))
  (dependents [_ declaration-id]
    (into #{} (comp (filter #(contains? (:plan/pins %) declaration-id)) (map :plan/id))
          (vals (:plans @state))))
  (set-name! [_ id named]
    (if-not (get-in @state [:plans id])
      {:error :plan/not-found}
      (do (swap! state update :names
                 (fn [ns]
                   (cond-> (into {} (remove (comp #{id} :plan/id val)) ns)
                     named (assoc (:name named) {:plan/id id :description (:description named)}))))
          nil)))
  (names [_] (:names @state)))

(defn store [] (->Memory (atom {:plans {} :names {}})))
