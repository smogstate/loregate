# Loregate

A read-only data access layer over an ontology. Callers ask for data in a small query
language that names business types, attributes and measures; Loregate checks the request
against the ontology, applies the caller's row and field rules, reads from query services
and Parquet datasets, and returns rows.

Status: early design, nothing implemented yet.

## License

[Apache-2.0](LICENSE)
