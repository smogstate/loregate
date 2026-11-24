(ns loregate.store.none
  "The default plan store: nothing persists (SPEC 11). Plans live in the run cache only."
  (:require [loregate.store.port :as port]))

(defrecord None []
  port/PlanStore
  (put-plan!  [_ _] nil)
  (get-plan   [_ _] nil)
  (dependents [_ _] #{})
  (set-name!  [_ _ _] {:error :store/not-supported})
  (names      [_] {}))

(defn store [] (->None))
