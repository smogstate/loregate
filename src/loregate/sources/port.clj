(ns loregate.sources.port
  "Port for sources (SPEC 4.2). An adapter is scan (DuckDB reads it inside the statement)
  or fetch (Loregate fetches rows and loads them as src_<alias>).")

(defprotocol Source
  (kind        [s]                    "The :binding/kind this adapter serves.")
  (check       [s binding]            "Compile time: nil, or a vector of errors as data.")
  (schema-hash [s binding]            "Part of hash-of the binding (SPEC 5.6).")
  (relation    [s source]             "{:scan \"<table expression>\"} or {:fetch true}.")
  (fetch       [s source args user]   "Fetch adapters only: {:rows … :as-of …} or {:error …}."))
