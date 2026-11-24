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

## Phase 2 — Rules (2026-11-24)

Done when the redaction-equivalence test (14.3) passes for generated queries and two users:
**passing** for three users (ana, bob, cleo), 150 generated queries plus fixture queries
(`clojure -M:test`, 30 tests, 236 assertions).

- Each source CTE has two levels: the inner casts true values; the outer applies the row
  rule as WHERE and masks fields with CASE. Attributes only rules read are in the inner
  level and in `:plan/inputs`, never in the output.
- User-level field rules are evaluated once per run by DuckDB itself (`:plan/checks`), so
  there is one evaluator; the booleans feed `{:visible rule}` parameters and `:hidden`.
  Row-level rules give `:partial`; layer row rules other than `true` give `:rows-filtered`.
- `[:user k]` is typed by what it is compared with and cast to it; an absent attribute is
  NULL, so the rule is false (fail closed). A layer-enforced type with no row rule is
  refused (`:compile/no-row-rule`).
- Redacted copies are written by hand in `dev/fixtures/redacted/<user>.edn`; the test
  compares rules over real data with no rules over each copy, and checks that crossing
  users makes it fail.
- Fixed: `list_contains(coll, x)` bound x's parameters before coll's (phase 1 bug); an
  unresolved operand crashed the type checker instead of reporting `:compile/unknown-name`.

## Phase 3 — HTTP and auth (2026-11-24)

Done when a client can go from describe to rows with a token and nothing else: **passing**
(`clojure -M:test`, 34 tests, 266 assertions; the HTTP test signs real RS256 tokens and
talks to the server over a socket).

- `loregate.auth.oidc`: RS256 against the issuer's JWKS (JDK crypto, no JWT library),
  issuer, audience, exp/nbf with leeway, truncated-groups refusal, claim map, principals.
  ponytail: RS256 only; add ES256 when an identity provider needs it.
- `loregate.api.http`: one handler (request map → response map) and a JDK `HttpServer` on
  virtual threads. A Lambda host wraps the same handler later. Unexpected exceptions become
  `{:error :internal}` with no detail.
- `loregate.api.wire`: EDN and JSON bodies; the JSON naming rules (SPEC 10). New dependency:
  `org.clojure/data.json` (pure Clojure).
- `describe`: types, attributes with `:visible?` true/false/:per-row for the caller (user-level
  rules evaluated by DuckDB, as in runs), links, definitions, commands; never bindings or rules.

## Phase 4 — Service bindings (2026-11-24)

Done when a query joining a fake service and a Parquet file passes 14.3 and 14.5:
**passing** (`clojure -M:test`, 40 tests, 293 assertions).

- `loregate.sources.service`: fetch adapter over a `Transport`; the shipped HTTP transport
  (contract in SPEC 12.1) is tested against a real socket; the fake in
  `test/loregate/fake_service.clj` pages, records calls, and can time out, break or vanish.
- Pushdown: accepted filters, never on an attribute with a row-level field rule, one per
  argument; enum values in stored form; params filled at run time. 8.3: a source whose
  pushed filter names a field hidden from the user is not called and yields no rows.
- Run: fetch sources in parallel on virtual threads, per-source timeout; rows checked
  against the output schema; row cap is an error, never truncation; caller's token passed
  through; `:as-of` is the response time.
- Binding selection also refuses a binding that lacks an attribute a rule reads.
- Not done: the total-run budget (30 s) and per-user concurrency limit (`:budget/busy`).

## Phase 5 — Graph plan store (2026-11-24)

Done when changing a fixture measure makes its dependents report stale, and only those:
**passing** (`clojure -M:test`, 43 tests, 313 assertions).

- `loregate.store.graph`: plan nodes and `depends_on` edges (declaration, pinned hash) in a
  DuckDB file; plan bodies as EDN. Survives a restart: a fresh process runs a stored plan by
  id without the query. One connection, serialised; one node. Loretile's graph is the
  multi-node target (its API is not fixed yet).
- The store port gained names with descriptions (`set-name!` takes `{:name :description}`,
  `names` lists them) for phase 6.
- Budgets: per-user concurrent runs (`:budget/busy`) and a whole-run time budget.
  ponytail: a timed-out run finishes in the background.

## Phase 6 — Named plans (2026-11-24)

Done when a named plan returns the same rows as running it by id: **passing**
(`clojure -M:test`, 45 tests, 328 assertions).

- `publish` / `unpublish` (`PUT`/`DELETE /api/v1/plans/{id}/name`), needing the role in
  `:publish/role` (unset → nobody) and a store that keeps names (none → 501).
- `run-named` (`POST /api/v1/named/{name}/run`); `describe` lists named plans with their
  output and params. Names follow tool-name rules so an agent interface can expose them.
- No MCP server: HTTP is the only transport in v1 (SPEC 10.1); an MCP adapter would wrap
  `describe`, `query`, `run` and `run-named` from `loregate.core`.
