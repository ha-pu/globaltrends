# Test Plan: globaltrends

## Overview

The test suite uses **testthat 3** (`Config/testthat/edition: 3`) and **withr** for state
management. All test files live in `tests/testthat/`. Helper utilities are in
`helper-db.R` (auto-loaded by testthat before every file) and `test_functions.r`
(sourced explicitly by files that need it).

---

## Test files

### `test-computations.R` — Score and DOI computation, data removal

| Test | What it checks |
|------|---------------|
| `compute_score1` | `compute_score()` writes the correct number of rows to `data_score` and emits a progress message for each location. |
| `compute_score2–4` | Input validation for `object`, `control`, and `locations` parameters. |
| `compute_score5–6` | Same validation for `compute_voi()`. |
| `compute_voi1` | `compute_voi()` writes worldwide-only rows and emits the expected message. |
| `compute_doi1` | `compute_doi()` writes the correct number of rows to `data_doi`. |
| `compute_doi2–5` | Input validation for `compute_doi()`. |
| `remove_data1–2` | `remove_data("batch_keywords", control/object = 1)` cascades through the full dependency chain and emits a message per table. |
| `remove_data3–5` | Input validation for `remove_data()`. |
| `remove_data6` | `vacuum_data()` emits the success message. |

**Gaps / recommended additions**

- Score formula correctness: with a controlled fixture, assert that `score == hits_o / sum(control_hits)` for a specific row.
- List-dispatch: verify that passing `object = list(1, 2)` to `compute_score()` and `compute_doi()` processes both batches.
- Cascade into `data_related` and `data_region`: `remove_data("data_object", object = 1)` should also emit messages for those tables.

---

### `test-metrics.R` — Internal dispersion metric functions *(new)*

Pure-function tests; no database required. All three functions are accessed via
`globaltrends:::` so the tests work under both `devtools::test()` and `R CMD check`.

#### `.compute_gini()`

| Test | Input | Expected |
|------|-------|---------|
| Uniform distribution | `rep(1, 5)` | `1` |
| Single non-NA element | `42` | `1` |
| Maximally concentrated | `c(1, 0, 0, 0, 0)` | `0.2` (= 1 − 4/5) |
| Two unequal values | `c(1, 3)` | `0.75` |
| All-NA input | `c(NA, NA)` | `NA` |
| All-zero input | `c(0, 0, 0)` | `0` |
| Mixed NA + non-NA | `c(1, NA, 1)` | `1` (NAs stripped → uniform) |

#### `.compute_hhi()`

| Test | Input | Expected |
|------|-------|---------|
| Uniform distribution | `rep(1, 5)` | `0.8` (= 1 − 1/5) |
| Monopoly | `c(1, 0, 0, 0, 0)` | `0` |
| Two unequal values | `c(1, 3)` | `0.375` |
| All-NA input | `c(NA, NA)` | `NA` |
| All-zero input | `c(0, 0, 0)` | `0` |
| Mixed NA + non-NA | `c(1, NA, 1)` | `0.5` |

#### `.compute_entropy()`

| Test | Input | Expected |
|------|-------|---------|
| Uniform distribution | `rep(1, 5)` | `0` |
| Single non-zero element | `42` | `0` |
| Zeros alongside non-zero | `c(5, 0, 0)` | `0` |
| All-NA input | `c(NA, NA)` | `NA` |
| All-zero input | `c(0, 0, 0)` | `0` |
| Non-uniform (negative result) | `c(2, 1)` | `−sum(p·log p) − log(2)` where `p = c(2/3, 1/3)` |
| Mixed NA + non-NA | `c(1, NA, 1)` | `0` |
| Monotone property | `c(1,2,3)`, `c(10,1,1,1)`, `c(50,30,20)` | All `≤ 0` |

---

### `test-export.R` — Data export functions

| Test | What it checks |
|------|---------------|
| `export_control1–5` | Row counts with NULL/scalar/list filters; input validation. |
| `export_control_global1–5` | Same for worldwide-only export. |
| `export_object1–7` | Row counts filtered by `keyword`, `object`, `control`. |
| `export_object_global1–7` | Same for worldwide-only export. |
| `export_score1–7` | Row counts filtered by `keyword`, `object`, `control`. |
| `export_voi1–7` | Row counts for worldwide scores. |
| `export_doi1–7,9` | Row counts filtered by `keyword`, `object`, `control`, `locations`. |

**Gaps / recommended additions**

- Column types: assert `date` column is `Date` class in all export functions.
- Column renaming: assert `control` and `object` columns are present and `batch_c` / `batch_o` are absent in the output of `export_object()`, `export_score()`, and `export_doi()`.
- Numeric columns: assert `gini`, `hhi`, `entropy` are `double` in `export_doi()`.

---

### `test-downloads.R` — Download functions *(currently all commented out)*

All tests in this file are disabled. The recommended structure once re-enabled:

| Test | Requires network | What it checks |
|------|-----------------|----------------|
| `download_object` before control data | No | `download_object()` emits an error message when no control data exists for the requested location. |
| `download_control` deduplication | No (pre-insert fixture) | A location already present in `data_control` is skipped with an "already available" message. |
| `download_control` happy path | Yes (`skip_if_offline()`) | Correct row count written; progress messages emitted per location. |
| Re-download control | Yes | Second call for the same location skips with deduplication message. |
| `download_control_global` | Yes | Worldwide aggregate written correctly. |
| `download_control` input validation | No | Errors on wrong types for `control` and `locations`. |
| `download_object` happy path | Yes | Correct row count; control keyword ranking heuristic selects the right pairing. |
| Re-download object | Yes | Second call skips already-present `(batch_c, batch_o, location)` triplet. |
| `download_object_global` | Yes | Worldwide aggregate written correctly. |
| `download_object` input validation | No | Errors on wrong types for `object`, `control`, `locations`. |

For offline-safe testing of the deduplication and guard logic, use
`local_mocked_bindings()` to stub `.get_trend()` rather than hitting the live API.

---

### `test-python.R` — Python backend setup

| Test | What it checks |
|------|---------------|
| *(see file)* | `initialize_python()` validates environment arguments, stores the API key in `gt.env`, and sets `gt.env$py_setup = TRUE`. |

---

## Priority order

| Priority | Item | Rationale |
|----------|------|-----------|
| **Done** | `test-metrics.R` | Scientific core; was at zero coverage |
| **Done** | Fix `test-synonyms.R` message patterns | Tests were checking stale strings and would error on `out[[7]]` |
| **Done** | Fix `test-computations.R` vacuum test | Referenced a non-existent `.sqlite` file |
| **Done** | `helper-db.R` | Enables self-sufficient DB setup for future tests |
| High | Re-enable `test-downloads.R` | Entire download layer is untested |
| Medium | Exact value check in `test-synonyms.R` | Qualitative "score increased" is weak |
| Medium | Column type/name checks in `test-export.R` | Currently only row counts are verified |
| Medium | `compute_score` value check | Row count does not verify the normalisation formula |
| Medium | `remove_data` cascade for `data_related`/`data_region` | These tables are in the dependency graph but not tested |
| Low | Parquet round-trip in `test-db_functions.R` | Verifies persistence across disconnect/reconnect |
| Low | List-dispatch paths for `compute_score` and `compute_doi` | Only scalar dispatch is currently exercised |
