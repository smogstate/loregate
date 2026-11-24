# Loregate

A read-only data access layer over an ontology. Callers ask for data in a small query
language that names business types, attributes and measures; Loregate checks the request
against the ontology, applies the caller's row and field rules, reads from query services
and Parquet datasets, and returns rows.

Status: v0.1, implemented end to end — query language, compiler, row and field rules,
Parquet and query-service sources, HTTP with OIDC tokens, plan stores, named plans,
conformance kits. Operators: see [docs/operators.md](docs/operators.md).

## Development

Needs the Clojure CLI and Java 21+.

```sh
clojure -M:test                 # run the tests
clojure -M:serve settings.edn   # serve over HTTP
clojure -M:uberjar              # build target/loregate.jar
clojure -M:json-schema out/     # JSON Schema of the contract
clojure -M:fixtures             # regenerate dev/fixtures/data/*.parquet from the .edn fixtures
```

## License

[Apache-2.0](LICENSE)
