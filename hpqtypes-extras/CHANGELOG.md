# hpqtypes-extras-1.21.0.0 (2026-09-18)
* `migrateDatabase` and `checkDatabase` now require PostgreSQL 15 or later. On an
  older server they stop with an error.
* Remove `checkAndRememberMaterializationSupport`. All supported servers
  understand the `MATERIALIZED` keyword, so delete the call from your code.
* `sqlWith` now emits a plain `WITH` clause and lets PostgreSQL decide whether
  to materialize it. It used to emit `WITH ... AS NOT MATERIALIZED`. To keep the
  old behavior, use the new `sqlWithNotMaterialized`.

# hpqtypes-extras-1.20.0.0 (2026-06-10)
* Drop `crypton` dependency in favor of `ppad-ripemd160`.
* Add support for customizing trigger functions.
* Adds a `Function` datatype and a `defaultTriggerFunction` backwards-compatible
  trigger function helper.
* Remove `Read` instance of `IndexMethod`.
* Add support for the `NUMERIC` column type.
* Fix getting the row estimate for `ModifyColumnMigration` if a table with the
  same name is present in multiple schemas.
* Add support for renaming an existing local index during a migration that
  creates index concurrently.
* Fix two bugs in overlapping indexes check (not excluding local indexes
  properly and query failure when the table doesn't exist).
* Drop redundant table name from `CreateIndexConcurrentlyMigration`,
  `DropIndexConcurrentlyMigration` and `ModifyColumnMigration`.

# hpqtypes-extras-1.19.0.0 (2025-11-27)
* Compatibility with `hpqtypes` >= 0.13.0.0.

# hpqtypes-extras-1.18.0.0 (2025-06-02)
* Don't consider invalid indexes when checking consistency of the database.
* Improve handling of lock failures during migrations.
* Rename `eoLockTimeoutMs` to `eoLockTimeoutSecs` to normalize units.

# hpqtypes-extras-1.17.0.1 (2025-03-27)
* Fix validation of NOT NULL domains with PostgreSQL >= 17.

# hpqtypes-extras-1.17.0.0 (2025-03-12)
* Grouped some parameters of `migrateDatabase` and `checkDatabase` into a
  `DatabaseDefinitions` record type.
* Add an optional check that all foreign keys have an index.
* Add support for NULLS NOT DISTINCT in unique indexes.
* Add `sqlAll` and `sqlAny` to allow creating `SQL` expressions with
  nested `AND` and `OR` conditions.
* Add `SqlWhereAll` and `SqlWhereAny` so they can be used in signatures.
* Add rudimentary support for enum types.
* Add support for some regular triggers, ie: `AFTER` triggers without constraints
  and `BEFORE` triggers.

# hpqtypes-extras-1.16.4.4 (2023-08-23)
* Switch from `cryptonite` to `crypton`.
* Make `sqlWhereEqualsAny`, `sqlWhereIn` and `sqlWhereNotIn` prepared-query
  friendly.

# hpqtypes-extras-1.16.4.3 (2023-06-12)
* Synchronize timezone of a session with timezone of a database after changing
  the latter.

# hpqtypes-extras-1.16.4.2 (2023-05-23)
* Make order of tables during schema creation irrelevant.

# hpqtypes-extras-1.16.4.1 (2023-05-15)
* Relax checks around indexes related to the `REINDEX` operation.

# hpqtypes-extras-1.16.4.0 (2023-04-20)
* Add support for the `UNION ALL` clause via `sqlUnionAll`.

# hpqtypes-extras-1.16.3.1 (2023-04-13)
* Add support for GHC 9.6.
* Fix checkAndRememberMaterializationSupport's query.

# hpqtypes-extras-1.16.3.0 (2023-01-25)
* Add support for `WITH MATERIALIZED` with backward compatibility.
* Add `sqlWhereEqualsAny`.

# hpqtypes-extras-1.16.2.0 (2022-10-27)
* Add support for setting collation method for columns.

# hpqtypes-extras-1.16.1.0 (2022-08-02)
* Add support for `sqlResult` in `sqlDelete`.
* Add a migration type for modifying columns.

# hpqtypes-extras-1.16.0.0 (2022-05-20)
* Trigger functions are now part of triggers and have their names generated.

# hpqtypes-extras-1.15.0.0 (2022-05-11)
* Add support for GHC 9.2.
* Drop support for GHC < 8.8.
* Add support for triggers and trigger functions.
* Allow to specify the operator class of an index column.

# hpqtypes-extras-1.14.1.0 (2022-01-11)
* Support unmanaged local indexes whose names start with "local_".

# hpqtypes-extras-1.14.0.0 (2021-12-13)
* Add support for ON CONFLICT to sqlInsertSelect.
* Remove upper bounds from library dependencies.
* Remove kRun* and kWhyNot functions.

# hpqtypes-extras-1.13.1.0 (2021-11-26)
* Add support for including columns in indexes.

# hpqtypes-extras-1.13.0.0 (2021-11-08)
* Add support for handling lock_timeout during migrations.
* Improvements for making no downtime migrations easier to write.
* Commiting after each migration was made non-optional.

# hpqtypes-extras-1.12.0.1 (2021-10-11)
* Add support for log-base-0.11.0.0

# hpqtypes-extras-1.12.0.0 (2021-09-29)
* Use plain exceptions instead of DBExtraException

# hpqtypes-extras-1.11.0.0 (2021-03-29)
* Support running with higher table versions in the database than in the code

# hpqtypes-extras-1.10.4.0 (2021-02-04)
* Generate valid INSERT SELECT query with data modifying WITH clauses
* Add DerivingVia helpers for enums

# hpqtypes-extras-1.10.3.0 (2020-11-16)
* Include LIMIT clause in UNION subqueries of the select

# hpqtypes-extras-1.10.2.1 (2020-05-05)
* Add support for GHC 8.10

# hpqtypes-extras-1.10.2.0 (2020-01-20)
* Add support for UNION clause
* Add support for GHC 8.8

# hpqtypes-extras-1.10.1.0 (2020-01-09)
* Add support for ON CONFLICT clause

# hpqtypes-extras-1.10.0.0 (2019-11-05)
* Implement `UuidT` Column Type ([#28](https://github.com/scrive/hpqtypes-extras/pull/28)).
* Fix sqlValidateCheck and sqlValidateFK

# hpqtypes-extras-1.9.0.1 (2019-06-04)
* Create composite types automatically only if database is empty
  ([#24](https://github.com/scrive/hpqtypes-extras/pull/24)).

# hpqtypes-extras-1.9.0.0 (2019-05-22)
* Extend `checkDatabaseAllowUnknownTables` to allow unknown composite
  types and rename it to `checkDatabaseAllowUnknownObjects`
  ([#22](https://github.com/scrive/hpqtypes-extras/pull/22)).
* Remove the `Default` instance for `ExtrasOptions`; use
  `defaultExtrasOptions` instead
  ([#23](https://github.com/scrive/hpqtypes-extras/pull/23)).

# hpqtypes-extras-1.8.0.0 (2019-04-30)
* Make composite types subject to migration process
  ([#21](https://github.com/scrive/hpqtypes-extras/pull/21)).
* Add a migration type for concurrent creation of an index
  ([#21](https://github.com/scrive/hpqtypes-extras/pull/21)).

# hpqtypes-extras-1.7.1.0 (2019-02-04)
* Fix an issue where unnecessary migrations were run sometimes
  ([#18](https://github.com/scrive/hpqtypes-extras/pull/18)).

# hpqtypes-extras-1.7.0.0 (2019-01-08)
* Added support for no-downtime migrations
  ([#17](https://github.com/scrive/hpqtypes-extras/pull/17)):
    - `sqlCreateIndex` is deprecated. Use either
      `sqlCreateIndexSequentially` or `sqlCreateIndexConcurrently`
      (no-downtime migration variant) instead.
    - `sqlAddFK` is deprecated. Use either `sqlAddValidFK` or
      `sqlAddNotValidFK` (no-downtime migration variant) instead.
    - API addition: `sqlValidateFK`, for validating a foreign key
      previously added with `sqlAddNotValidFK`.
    - `sqlAddCheck` is deprecated. Use either `sqlAddValidCheck` or
      `sqlAddNotValidCheck` (no-downtime migration variant) instead.
    - API addition: `sqlValidateCheck`, for validating a check
      previously added with `sqlAddNotValidCheck`.
    - API addition: `sqlAddPKUsing`, converts a unique index to a
      primary key.
    - New `Table` field: `tblAcceptedDbVersions`.
* `ValidationResult` is now an abstract type.
* `ValidationResult` now supports info-level messages in addition to errors.

# hpqtypes-extras-1.6.4.0 (2019-02-04)
* Fix an issue where unnecessary migrations were run sometimes
  ([#19](https://github.com/scrive/hpqtypes-extras/pull/19)).

# hpqtypes-extras-1.6.3.0 (2018-11-19)
* API addition: `sqlWhereAnyE`
  ([#16](https://github.com/scrive/hpqtypes-extras/pull/16)).

# hpqtypes-extras-1.6.2.0 (2018-07-11)
* Support hpqtypes-1.6.0.0.
* Drop support for GHC < 8.

# hpqtypes-extras-1.6.1.0 (2018-03-18)
* Add support for GHC 8.4.
* Drop support for GHC 7.8.

# hpqtypes-extras-1.6.0.0 (2018-01-25)
* Introduce `checkPKPresence` to enforce primary keys on all tables supplied to `checkDatabase`
* Introduce an options data type, `ExtrasOptions`

# hpqtypes-extras-1.5.0.1 (2018-01-09)
* Changed `getDBTableNames` to only schemas explicitly in search path, rather
  than an exclusion list. Affects table version and unknown tables checks.

# hpqtypes-extras-1.5.0.0 (2017-12-08)
* Changed internal representation of PrimaryKey to NubList (#11)
  This will break existing PKs set on multiple columns unless they are
  alphabetically sorted in the defining list.

# hpqtypes-extras-1.4.0.0 (2017-11-24)
* Introduced tsvector postgres type and indexing methods GIN and BTree

# hpqtypes-extras-1.3.1.1 (2017-07-21)
* Now depends on 'log-base' instead of 'log'.

# hpqtypes-extras-1.3.1.0 (2017-07-20)
* Improved migration order sanity checking (#7).

# hpqtypes-extras-1.3.0.0 (2017-05-17)
* Add drop table migrations.
* Add a test suite.
* Improve documentation.
* Add option to force commit after every migration.

# hpqtypes-extras-1.2.4 (2016-07-28)
* Initial release.
