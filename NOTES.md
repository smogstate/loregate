# Notes

One entry per delivery phase.

## Phase 0 — Schemas and fixtures (2026-11-24)

Done when every spec example validates and generated queries validate: **passing**
(`clojure -M:test`, 12 tests, 131 assertions).

- `loregate.schema.{expr,query,ontology,plan,result}` (cljc, malli) with generators.
  Expressions come in three variants: query (`[:param k]`), rule (`[:user k]`), definition
  (neither).
- Literal collections are sets (`#{…}`): a vector of qualified keywords is always a path.
- malli cannot generate a recursive ref inside a sequence schema, so variadic operators
  validate as `:cat` and generate from a tuple-only copy of the grammar (`:gen/schema`).
- Generated values are syntactically valid only. Queries that are valid against the
  fixture ontology need a generator that picks names from it (phase 1/2).
- Ports: `Ontology`, `Source`, `PlanStore`, `Identity`. The in-memory ontology adapter
  refuses an invalid ontology and reports every problem (schema, duplicate ids, conflicting
  attribute declarations, dangling references, enum fields without value mappings).
- Attribute names are global: a field rule on `:biz/client-id` applies on every type that
  has it, and every type must declare it the same way.
- Fixtures: invoices (service + Parquet bindings), clients (row rule), contracts (per-row
  grants). Parquet is generated from `dev/fixtures/data.edn` with `clojure -M:fixtures`.
