# Loregate

A read-only data access layer over an ontology. Callers ask for data in a small query
language that names business types, attributes and measures; Loregate checks the request
against the ontology, applies the caller's row and field rules, reads from query services
and Parquet datasets, and returns rows.

Status: early. Schemas, ports and fixtures exist; nothing compiles or runs queries yet.

## Development

Needs the Clojure CLI and Java 21+.

```sh
clojure -M:test       # run the tests
clojure -M:fixtures   # regenerate dev/fixtures/data/*.parquet from dev/fixtures/data.edn
```

## License

[Apache-2.0](LICENSE)
