(ns loregate.schema.result
  "What a run returns (SPEC 9) and the closed set of run errors."
  (:require [loregate.schema.plan :as plan]))

(def Notes
  [:map {:closed true}
   [:plan plan/Hash]
   [:as-of [:map-of simple-keyword? inst?]]
   [:clock [:map {:closed true}
            [:now inst?]
            [:today [:re #"^\d{4}-\d{2}-\d{2}$"]]
            [:zone string?]]]
   [:hidden [:set qualified-keyword?]]
   [:partial [:set qualified-keyword?]]
   [:rows-filtered [:set qualified-keyword?]]])

(def Result
  [:map {:closed true}
   [:rows [:vector [:map-of [:or qualified-keyword? simple-keyword?] any?]]]
   [:columns [:vector plan/Column]]
   [:notes Notes]])

(def run-errors
  #{:auth/missing-token :auth/invalid-token :auth/groups-truncated
    :plan/not-found :plan/stale
    :params/invalid
    :source/timeout :source/unavailable :source/schema-mismatch :source/row-cap-exceeded
    :budget/busy
    :store/not-supported})

(def RunError
  [:map
   [:error (into [:enum] (sort run-errors))]])
