(ns loregate.auth.port
  "Port for identity (SPEC 8.1).")

(defprotocol Identity
  (authenticate [i request]
    "{:user/id … :user/attrs {…} :user/credential …}, or {:error :auth/…}.
     :user/credential is what source adapters pass through (SPEC 12.1)."))

(def User
  "The user context every adapter must yield. :principals is \"user:<id>\" plus
  \"group:<g>\" for each group (SPEC 8.1)."
  [:map
   [:user/id string?]
   [:user/attrs [:map
                 [:principals [:set string?]]]]
   [:user/credential {:optional true} any?]])
