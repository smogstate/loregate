(ns loregate.store.memory
  "Plans, names and dependency edges in memory, until restart (SPEC 11)."
  (:require [loregate.store.port :as port]))

(defrecord Memory [state]                     ; atom {:plans {id plan} :names {name id}}
  port/PlanStore
  (put-plan! [_ plan]
    (swap! state assoc-in [:plans (:plan/id plan)] plan)
    nil)
  (get-plan [_ id]
    (get-in @state [:plans id]))
  (dependents [_ declaration-id]
    (into #{} (comp (filter #(contains? (:plan/pins %) declaration-id)) (map :plan/id))
          (vals (:plans @state))))
  (set-name! [_ id name-or-nil]
    (cond
      (not (get-in @state [:plans id])) {:error :plan/not-found}
      (nil? name-or-nil) (do (swap! state update :names #(into {} (remove (comp #{id} val)) %)) nil)
      :else (do (swap! state assoc-in [:names name-or-nil] id) nil))))

(defn store [] (->Memory (atom {:plans {} :names {}})))
