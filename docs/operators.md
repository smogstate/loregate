# Running Loregate

Loregate is a read-only data layer over an ontology. A deployment brings its own ontology,
identity provider and data; nothing about them is code. This guide covers what an operator
sets up.

## Build and run

```sh
clojure -M:uberjar                                   # target/loregate.jar (Clojure + JDK only)
java --enable-native-access=ALL-UNNAMED -jar target/loregate.jar settings.edn
# or, without building:  clojure -M:serve settings.edn
# or, as a container:    see Dockerfile
```

Java 21 or later. The server listens on `:http :port` (default 8080) and serves `/api/v1/…`.
Put TLS, cross-origin rules and rate limits in front of it (reverse proxy, API gateway).

## Settings

One EDN file, read at start, with environment variables over it. A variable overrides a
setting that exists: `LOREGATE_` plus the setting's path, upper-cased, with `_` for any
other character — `[:http :port]` → `LOREGATE_HTTP_PORT`, `:storage/allowed` →
`LOREGATE_STORAGE_ALLOWED`. Values are read as EDN (`8080`, `["s3://lake/"]`, `true`);
anything else is a string. Secrets need no setting: Loregate holds no service credentials,
and identity is verified against the provider's public keys.

```clojure
{:ontology {:adapter :edn :path "ontology.edn"}          ; the declarations (types, bindings, rules…)
 :store    {:adapter :graph :path "plans.db"}            ; :none (default) | :memory | :graph
 :storage/allowed ["s3://lake/" "/data/"]                 ; the only places Parquet bindings may read
 :sources  {:parquet {}
            :service {:bases {"invoicing" "https://invoicing.internal"}}}
 :identity {:adapter :oidc
            :issuer "https://idp.example.com/"
            :audience "loregate"
            :claims {:roles "roles" :region "region" :groups "groups"}}  ; user attribute → claim
 :publish/role "role:report-admin"                        ; who may name plans; nil → nobody
 :http {:host "0.0.0.0" :port 8080}}
```

Engine settings and their defaults are in `src/loregate/config.clj` (row and limit
maximums, time zone, DuckDB memory and threads, source and run timeouts, concurrent runs
per user).

## Identity

Tokens are OIDC JWTs signed RS256. Loregate discovers the provider's keys from
`<issuer>/.well-known/openid-configuration` (or `:jwks-uri`), checks issuer, audience,
expiry and not-before, and maps claims to user attributes with `:claims`. Rules read those
attributes as `[:user :roles]`, `[:user :region]`…; `[:user :principals]` is computed as
`user:<sub>` plus `group:<g>` for each group. A token whose provider reports a truncated
group list is refused. The same token is passed to query services, so they must accept the
same issuer and audience.

## Data sources

- **Parquet** (`:binding/kind :parquet`, `:parquet/path`): read with DuckDB, under the
  `:storage/allowed` prefixes only. Rows of these types are guarded by the ontology's row
  rules alone: a type with no row rule cannot be queried.
- **Query services** (`:binding/kind :service`, `:service/name`, `:service/query`): called
  over HTTP as the user. Each query must publish its row's JSON Schema at
  `GET <base>/queries/<query>/schema` and answer `POST <base>/queries/<query>` with
  `{"args": {…}, "cursor": c}` → `{"rows": […], "next": c|null}`.

## Plans

With the default `:none` store nothing persists; clients re-send their query with a run.
`:graph` keeps plans, their dependencies and names in a DuckDB file (one node). Plans go
stale when a declaration they pinned changes, or when Loregate's compiler version changes;
`GET /api/v1/plans/{id}` says which.

## Adding an adapter

Ports: `loregate.ontology.port/Ontology`, `loregate.store.port/PlanStore`,
`loregate.sources.port/Source`, `loregate.auth.port/Identity`. A new adapter must pass its
kit in `loregate.conformance` (each returns a vector of failures; empty means it conforms).
A Source adapter must also pass `redaction-kit`: rules over real data equal no rules over a
copy redacted by hand. Operators and the query language are not extensible.

## Clients outside Clojure

`clojure -M:json-schema out/` writes the JSON Schema of the query, result, errors and
columns. JSON naming rules and the wire encoding are in the spec, section 10.
