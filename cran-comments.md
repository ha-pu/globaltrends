# Update 0.2.1

## Test environments
* local Windows 11 x64 (build 26200), R 4.6.0
* R-hub: Linux (R-devel), Windows (R-devel), macOS arm64 (R-devel)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Reverse dependencies
There are currently no reverse dependencies for this package on CRAN.

## Use of \dontrun{} in examples
All exported function examples are wrapped in \dontrun{} because they require
either an active database connection (initialized via `initialize_db()` and
`start_db()`) or a valid Google Trends API key (set up via
`initialize_python()`). These prerequisites cannot be satisfied in an automated
check environment.

---

This is a major update of the globaltrends package. Key changes include:

* General revision and improvement of code base, tests, and documentation
* Switch from SQLite to RDS for database storage
* Inclusion of API-usage counter for downloads through Google Trends API
* Improved error handling for Google Trends API calls
* Updated dependency to testthat 3.0.0

Thanks!
Harald Puhr
