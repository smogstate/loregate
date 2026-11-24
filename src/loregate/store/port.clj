(ns loregate.store.port
  "Port for plans and their dependencies (SPEC 11).")

(defprotocol PlanStore
  (put-plan!  [s plan]             "Idempotent by :plan/id.")
  (get-plan   [s id]               "The plan, or nil.")
  (dependents [s declaration-id]   "Ids of the plans that pin this declaration.")
  (set-name!  [s id name-or-nil]   "Name or unname a plan; {:error :store/not-supported} if the
                                    adapter keeps no names."))
