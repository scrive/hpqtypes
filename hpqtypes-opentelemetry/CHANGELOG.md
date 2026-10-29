# hpqtypes-opentelemetry-0.1.0.0 (????-??-??)
* Initial release.
* Spans are named after a summary of the query, e.g. `INSERT
  high_value_customers SELECT customers` or `CALL some_procedure`, which
  `Database.PostgreSQL.PQTypes.OpenTelemetry.QuerySummary` extracts from the
  text of the query on a best-effort basis.
