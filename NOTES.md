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

## Phase 1 — Compile and run over Parquet, no rules (2026-11-24)

Done when the fixture queries return the expected rows and plan ids are stable: **passing**
(`clojure -M:test`, 25 tests, 195 assertions, plus 100 generated queries).

- `loregate.compile.*`: resolve (names → IR, expanding definitions, measures and locals),
  typecheck, binding selection, SQL. Every value is a positional parameter cast to the type
  its context needs (`CAST(? AS DATE)`); DuckDB will not compare a date with an untyped one.
  Descriptors in `:plan/sql-params`.
- Every output column is cast to its declared type: DuckDB returns integer sums as
  BigInteger and decimal averages/divisions as doubles. A decimal definition keeps the scale
  of what it computes.
- Each source is a CTE over `src_<alias>`: Parquet is attached as a view, supplied rows
  (`:relations`, library only) as a table. Unmapped enum values hit `error()` in the CTE and
  surface as `:source/schema-mismatch`.
- JSON fields are inlined as constant paths: DuckDB rejects wildcard paths as parameters.
  A `[*]` path over a missing key yields an empty set.
- Implicit row limit: no `LIMIT` is compiled when the query sets none (settings never change
  plans); the run reads at most `:max-rows` + 1 rows and notes `:truncated`.
- `loregate.core`: `compile`, `run` (cache → store → recompile from the query), `query`,
  `plan-status`. Stores: none (default) and memory. Plans carry `:plan/compiler`.
- Deferred: `datetime` is UTC only (no TIMESTAMPTZ yet); supplied rows cannot fill JSON
  fields; the plan cache is unbounded; DuckDB external access is limited by the Parquet
  adapter's allowed prefixes, not by DuckDB settings yet; per-run timeout.
