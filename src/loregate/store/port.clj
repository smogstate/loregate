(ns loregate.store.port
  "Port for plans and their dependencies (SPEC 11).")

(defprotocol PlanStore
  (put-plan!  [s plan]             "Idempotent by :plan/id.")
  (get-plan   [s id]               "The plan, or nil.")
  (dependents [s declaration-id]   "Ids of the plans that pin this declaration.")
  (set-name!  [s id named]         "Name a plan with {:name … :description …} (a name held by
                                    another plan moves), or unname it with nil. nil on
                                    success, {:error :store/not-supported} if the adapter
                                    keeps no names, {:error :plan/not-found} for an unknown id.")
  (names      [s]                  "{name {:plan/id … :description …}} of every named plan."))
