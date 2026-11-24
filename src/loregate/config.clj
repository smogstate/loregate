(ns loregate.config
  "The one list of settings (SPEC 4.2). Reading a settings file with environment
  overrides comes with packaging (phase 7).")

(def defaults
  {:max-limit 10000                 ; largest :limit a query may ask for
   :max-rows  10000                 ; rows returned when a query sets no :limit
   :zone      "UTC"                 ; time zone of [:now] and [:today]
   :duckdb    {:memory-limit "1GB" :threads 4}
   :source-timeout-ms 10000         ; per fetch source
   :storage/allowed []})            ; location prefixes Parquet bindings may read
