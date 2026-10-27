# hpqtypes-effectful-1.2.0.0 (2026-09-28)
* Compatibility with `hpqtypes` >= 1.15.0.0.
* Rename the `WithNewConnection` constructor of the `DB` effect to
  `WithNewSession`, to match the rename of `withNewConnection` to
  `withNewSession` in `hpqtypes`.
* A database session is now bound to the thread that started it. If another
  thread runs a query in that session, the query throws `ThreadMismatchError`.
  To run queries from another thread, start a separate session there with
  `withNewSession`.

# hpqtypes-effectful-1.1.0.0 (2025-11-27)
* Compatibility with `hpqtypes` >= 1.13.0.0.

# hpqtypes-effectful-1.0.2.0 (2024-03-18)
* Compatibility with `hpqtypes` >= 1.12.0.0.

# hpqtypes-effectful-1.0.1.0 (2023-12-18)
* ~~Compatibility with `hpqtypes` >= 1.12.0.0.~~

# hpqtypes-effectful-1.0.0.1 (2023-03-28)
* Compatibility with `hpqtypes` >= 1.11.1.0.

# hpqtypes-effectful-1.0.0.0 (2022-10-24)
* Initial release.
