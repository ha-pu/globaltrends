#' Initialize the local database store
#'
#' Creates the local database store used by `globaltrends` in the current
#' working directory and initializes all required tables.
#'
#' @details
#' The package uses `data.table` objects persisted as a single RDS file under
#' the `db/` folder. `initialize_db()` creates 10 empty tables, populates
#' default location sets, and saves the result to `db/globaltrends.rds`.
#'
#' If the RDS file already exists the function returns early without
#' overwriting anything.
#'
#' Default location sets written to `data_locations`:
#' \describe{
#'   \item{`countries`}{ISO 3166-1 alpha-2 codes for countries above the GDP
#'     share threshold (see [countries]).}
#'   \item{`us_states`}{ISO 3166-2 codes for US states and Washington DC
#'     (see [us_states]).}
#' }
#'
#' @return Invisibly returns `TRUE`. Called for its side effects (creating
#'   files under `db/`).
#'
#' @seealso [start_db()] to open a working session after initialization;
#'   [disconnect_db()] to persist changes and close the session.
#'
#' @examples
#' \dontrun{
#' initialize_db()
#' start_db()
#' }
#'
#' @export

initialize_db <- function() {
  .ensure_db_dir()

  rds_path <- file.path("db", "globaltrends.rds")
  if (file.exists(rds_path)) {
    message("Database files already exist under 'db/'.")
    return(invisible(TRUE))
  }

  db <- list(
    batch_keywords = data.table::data.table(
      type = character(), batch = integer(), keyword = character()
    ),
    batch_time = data.table::data.table(
      type = character(), batch = integer(),
      start_date = character(), end_date = character()
    ),
    data_control = data.table::data.table(
      location = character(), keyword = character(),
      date = numeric(), hits = numeric(), batch = integer()
    ),
    data_object = data.table::data.table(
      location = character(), keyword = character(),
      date = numeric(), hits = numeric(),
      batch_c = integer(), batch_o = integer()
    ),
    data_score = data.table::data.table(
      location = character(), keyword = character(),
      date = numeric(), score = numeric(),
      batch_c = integer(), batch_o = integer()
    ),
    data_doi = data.table::data.table(
      keyword = character(), date = numeric(),
      gini = numeric(), hhi = numeric(), entropy = numeric(),
      batch_c = integer(), batch_o = integer(), locations = character()
    ),
    data_locations = data.table::data.table(
      location = character(), type = character()
    ),
    data_region = data.table::data.table(
      term = character(), location = character(),
      start_date = numeric(), end_date = numeric(),
      region_code = character(), region_name = character(),
      hits = numeric(), batch_o = integer()
    ),
    data_related = data.table::data.table(
      term = character(), topic = integer(), rising = integer(),
      location = character(),
      start_date = numeric(), end_date = numeric(),
      related_term = character(), hits = numeric(), batch_o = integer()
    ),
    keyword_synonyms = data.table::data.table(
      keyword = character(), synonym = character()
    )
  )

  data_to_add <- data.table::data.table(
    location = c(globaltrends::countries, globaltrends::us_states),
    type = c(
      rep("countries", length(globaltrends::countries)),
      rep("us_states", length(globaltrends::us_states))
    )
  )
  db$data_locations <- data_to_add

  .save_db(db, file.path("db", "globaltrends.rds"))

  message("Database files created successfully under 'db/'.")
  invisible(TRUE)
}

# -------------------------------------------------------------------------
# Internal helpers for DB filesystem layout
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd

.ensure_db_dir <- function(path = "db") {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd

.list_files <- function() {
  c(
    "batch_keywords",
    "batch_time",
    "data_control",
    "data_doi",
    "data_locations",
    "data_object",
    "data_score",
    "data_region",
    "data_related",
    "keyword_synonyms"
  )
}

#' @keywords internal
#' @noRd

.table_slot <- function(table) {
  map <- c(
    batch_keywords   = "dt_keywords",
    batch_time       = "dt_time",
    data_control     = "dt_control",
    data_object      = "dt_object",
    data_score       = "dt_score",
    data_doi         = "dt_doi",
    data_locations   = "dt_locations",
    data_region      = "dt_region",
    data_related     = "dt_related",
    keyword_synonyms = "dt_synonyms"
  )
  map[[table]]
}

#' @keywords internal
#' @noRd

.save_db <- function(db = NULL, path = "db/globaltrends.rds") {
  if (is.null(db)) {
    db <- list(
      batch_keywords   = gt.env$dt_keywords,
      batch_time       = gt.env$dt_time,
      data_control     = gt.env$dt_control,
      data_object      = gt.env$dt_object,
      data_score       = gt.env$dt_score,
      data_doi         = gt.env$dt_doi,
      data_locations   = gt.env$dt_locations,
      data_region      = gt.env$dt_region,
      data_related     = gt.env$dt_related,
      keyword_synonyms = gt.env$dt_synonyms
    )
  }
  tmp <- paste0(path, ".tmp")
  saveRDS(db, tmp, compress = FALSE)
  file.rename(tmp, path)
  invisible(TRUE)
}

#' @keywords internal
#' @noRd

.load_db <- function(path = "db/globaltrends.rds") {
  readRDS(path)
}

# -------------------------------------------------------------------------
# Start / Stop lifecycle
# -------------------------------------------------------------------------

#' Start a database session
#'
#' Loads the RDS-backed store under `db/` into `data.table` objects in
#' `gt.env`.
#'
#' @details
#' Requires [initialize_db()] to have been run in the current working
#' directory. All tables are read from `db/globaltrends.rds` and assigned
#' into `gt.env` as `dt_*` bindings. Keys are set on the large tables for
#' fast lookups. Cached data frames for frequently-used metadata
#' (`keywords_control`, `keywords_object`, etc.) are also populated.
#' Location sets are exported as named character vectors via
#' `.export_locations()`.
#'
#' @return Invisibly returns `TRUE`. Called primarily for its side effects.
#'
#' @seealso [initialize_db()] to create the store before the first session;
#'   [disconnect_db()] to persist changes and close the session.
#'
#' @examples
#' \dontrun{
#' start_db()
#' # ... downloads and computations ...
#' disconnect_db()
#' }
#'
#' @export

start_db <- function() {
  rds_path <- file.path("db", "globaltrends.rds")
  if (!file.exists(rds_path)) {
    stop(
      "Database files do not exist under 'db/'. Run `initialize_db()` first.",
      call. = FALSE
    )
  }

  db <- .load_db(rds_path)

  gt.env$dt_keywords  <- data.table::setDT(db$batch_keywords)
  gt.env$dt_time      <- data.table::setDT(db$batch_time)
  gt.env$dt_control   <- data.table::setDT(db$data_control)
  gt.env$dt_object    <- data.table::setDT(db$data_object)
  gt.env$dt_score     <- data.table::setDT(db$data_score)
  gt.env$dt_doi       <- data.table::setDT(db$data_doi)
  gt.env$dt_locations <- data.table::setDT(db$data_locations)
  gt.env$dt_region    <- data.table::setDT(db$data_region)
  gt.env$dt_related   <- data.table::setDT(db$data_related)
  gt.env$dt_synonyms  <- data.table::setDT(db$keyword_synonyms)

  if (nrow(gt.env$dt_control) > 0L) {
    data.table::setkey(gt.env$dt_control, batch, location)
  }
  if (nrow(gt.env$dt_object) > 0L) {
    data.table::setkey(gt.env$dt_object, batch_c, batch_o, location)
  }
  if (nrow(gt.env$dt_score) > 0L) {
    data.table::setkey(gt.env$dt_score, batch_c, batch_o, location)
  }
  if (nrow(gt.env$dt_doi) > 0L) {
    data.table::setkey(gt.env$dt_doi, batch_c, batch_o, locations)
  }
  if (nrow(gt.env$dt_locations) > 0L) {
    data.table::setkey(gt.env$dt_locations, type, location)
  }

  gt.env$keywords_control <- as.data.frame(
    gt.env$dt_keywords[gt.env$dt_keywords$type == "control", c("batch", "keyword")]
  )
  gt.env$keywords_object <- as.data.frame(
    gt.env$dt_keywords[gt.env$dt_keywords$type == "object", c("batch", "keyword")]
  )
  gt.env$time_control <- as.data.frame(
    gt.env$dt_time[gt.env$dt_time$type == "control", c("batch", "start_date", "end_date")]
  )
  gt.env$time_object <- as.data.frame(
    gt.env$dt_time[gt.env$dt_time$type == "object", c("batch", "start_date", "end_date")]
  )
  gt.env$keyword_synonyms <- as.data.frame(gt.env$dt_synonyms)

  .export_locations()

  message("Successfully loaded database and exported table handles to gt.env.")
  invisible(TRUE)
}

#' Disconnect from the database and persist changes
#'
#' Exports the current in-memory state to the RDS store under `db/` and
#' clears table handles from `gt.env`.
#'
#' @details
#' Call this function after all downloads and computations are complete. It
#' overwrites `db/globaltrends.rds` with the current in-memory state.
#' All `dt_*` handles in `gt.env` are set to `NULL` afterwards.
#'
#' Data modified during the session will be **lost** if this function is not
#' called before the R session ends.
#'
#' @return Invisibly returns `TRUE`. Called for its side effects (writing
#'   files under `db/` and clearing handles).
#'
#' @seealso [initialize_db()] to create the store; [start_db()] to open a
#'   new session.
#'
#' @examples
#' \dontrun{
#' start_db()
#' # ... downloads and computations ...
#' disconnect_db()
#' }
#'
#' @export

disconnect_db <- function() {
  if (is.null(gt.env$dt_control)) {
    stop(
      "No active database session found in `gt.env`.",
      call. = FALSE
    )
  }

  .save_db()

  nulls <- list(
    dt_keywords  = NULL,
    dt_time      = NULL,
    dt_control   = NULL,
    dt_object    = NULL,
    dt_score     = NULL,
    dt_doi       = NULL,
    dt_locations = NULL,
    dt_region    = NULL,
    dt_related   = NULL,
    dt_synonyms  = NULL
  )
  invisible(list2env(nulls, envir = gt.env))

  message("Successfully disconnected and persisted database to 'db/'.")
  invisible(TRUE)
}
