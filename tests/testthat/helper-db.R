# Auto-loaded by testthat before every test file.
# Provides local_db() for fully-isolated database setup in a temp directory.
# Cleanup order (LIFO): disconnect_db → restore CWD → delete temp dir.

local_db <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_dir(dir, .local_envir = env)
  suppressMessages(initialize_db())
  suppressMessages(start_db())
  withr::defer(suppressMessages(disconnect_db()), envir = env)
  invisible(dir)
}

# Appends a data.frame subset of an example_* dataset to a gt.env table.
seed_table <- function(slot, data) {
  gt.env[[slot]] <- data.table::rbindlist(
    list(gt.env[[slot]], data.table::as.data.table(data)),
    use.names = TRUE
  )
  invisible(NULL)
}

# Extends local_db() with pre-computed example data in dt_control, dt_object,
# dt_score, and dt_doi — the read-only input the export_* tests filter on.
# Same subsets the legacy top-level seeding used: control batch 1 and object
# batches 1:3 for US, CN, and world; DOI for the "countries" location set.
local_export_db <- function(env = parent.frame()) {
  local_db(env = env)
  seed_table("dt_control", example_control[
    example_control$batch == 1 &
      example_control$location %in% c("US", "CN", "world"),
  ])
  seed_table("dt_object", example_object[
    example_object$batch_c == 1 &
      example_object$batch_o %in% 1:3 &
      example_object$location %in% c("US", "CN", "world"),
  ])
  seed_table("dt_score", example_score[
    example_score$batch_c == 1 &
      example_score$batch_o %in% 1:3 &
      example_score$location %in% c("US", "CN", "world"),
  ])
  seed_table("dt_doi", example_doi[
    example_doi$batch_c == 1 &
      example_doi$batch_o %in% 1:3 &
      example_doi$locations == "countries",
  ])
  invisible(NULL)
}

# Extends local_db() with keyword batches and raw download data (control batch
# 1 plus the requested object batches) for US, CN, JP, and world — the input
# compute_score()/compute_voi()/compute_doi() consume. No scores are computed;
# tests trigger computation themselves.
local_score_input_db <- function(object_batches = 1L, env = parent.frame()) {
  local_db(env = env)
  suppressMessages({
    add_control_keyword(
      keyword    = c("gmail", "map", "translate", "wikipedia", "youtube"),
      start_date = "2010-01",
      end_date   = "2019-12"
    )
    add_object_keyword(
      keyword    = c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
      start_date = "2010-01",
      end_date   = "2019-12"
    )
    if (2L %in% object_batches) {
      kws2 <- example_keywords[
        example_keywords$type == "object" & example_keywords$batch == 2,
      ]$keyword
      add_object_keyword(keyword = kws2, start_date = "2010-01", end_date = "2019-12")
    }
  })
  seed_table("dt_control", example_control[
    example_control$batch == 1 &
      example_control$location %in% c("US", "CN", "JP", "world"),
  ])
  seed_table("dt_object", example_object[
    example_object$batch_c == 1 &
      example_object$batch_o %in% object_batches &
      example_object$location %in% c("US", "CN", "JP", "world"),
  ])
  invisible(NULL)
}

# Extends local_score_input_db() with computed scores and DOI for the given
# object batches, plus one dt_related and one dt_region row per batch — a
# fully-populated database for remove_data() cascade tests.
local_cascade_db <- function(object_batches = 1L, env = parent.frame()) {
  local_score_input_db(object_batches = object_batches, env = env)
  suppressMessages({
    compute_score(
      object = as.list(object_batches),
      control = 1,
      locations = c("US", "CN", "JP")
    )
    compute_voi(object = as.list(object_batches), control = 1)
    compute_doi(
      object = as.list(object_batches),
      control = 1,
      locations = "countries"
    )
  })
  for (b in object_batches) {
    seed_table("dt_related", data.table::data.table(
      term = "fc barcelona", topic = 0L, rising = 0L,
      location = "world", start_date = as.Date("2019-01-01"),
      end_date = as.Date("2019-12-31"),
      related_term = "barcelona", hits = 100.0, batch_o = as.integer(b)
    ))
    seed_table("dt_region", data.table::data.table(
      term = "fc barcelona", location = "world",
      start_date = as.Date("2019-01-01"), end_date = as.Date("2019-12-31"),
      region_code = "ES-CT", region_name = "Catalonia",
      hits = 100.0, batch_o = as.integer(b)
    ))
  }
  invisible(NULL)
}

# Builds a minimal one-location, one-date score input: control batch 1 with
# keyword "c1", object batch 1 with keywords c("c1", "o1") ("c1" is the
# overlap term used for benchmarking). Hits are set per argument.
local_minimal_score_db <- function(ctrl_hits, obj_overlap_hits, obj_hits,
                                   env = parent.frame()) {
  local_db(env = env)
  suppressMessages({
    add_control_keyword(keyword = "c1", start_date = "2020-01", end_date = "2020-01")
    add_object_keyword(keyword = c("c1", "o1"), start_date = "2020-01", end_date = "2020-01")
  })
  seed_table("dt_control", data.frame(
    location = "US", keyword = "c1", date = as.Date("2020-01-01"),
    hits = ctrl_hits, batch = 1L
  ))
  seed_table("dt_object", data.frame(
    location = c("US", "US"), keyword = c("c1", "o1"),
    date = as.Date("2020-01-01"),
    hits = c(obj_overlap_hits, obj_hits),
    batch_c = 1L, batch_o = 1L
  ))
  invisible(NULL)
}

# Extends local_db() with the keyword batches, raw download data, and computed
# scores needed by the synonym tests.

local_synonyms_db <- function(env = parent.frame()) {
  local_db(env = env)
  suppressMessages({
    add_control_keyword(
      keyword    = c("gmail", "map", "wikipedia", "youtube"),
      start_date = "2010-01",
      end_date   = "2019-12"
    )
    add_object_keyword(
      keyword = list(
        c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
        c("bayern munich", "bayern munchen")
      ),
      start_date = "2010-01",
      end_date = "2019-12"
    )

    ctrl_data <- example_control[example_control$batch == 1 & example_control$location %in% c("US", "CN", "JP"), ]
    gt.env$dt_control <- data.table::rbindlist(
      list(gt.env$dt_control, data.table::as.data.table(ctrl_data)),
      use.names = TRUE
    )

    obj1 <- example_object[example_object$batch_c == 1 & example_object$batch_o == 1 & example_object$location %in% c("US", "CN"), ]
    obj1$batch_o <- 1L
    gt.env$dt_object <- data.table::rbindlist(
      list(gt.env$dt_object, data.table::as.data.table(obj1)),
      use.names = TRUE
    )

    obj2 <- example_object[example_object$batch_c == 1 & example_object$batch_o == 2 & example_object$location %in% c("CN", "JP"), ]
    obj2$batch_o <- 2L
    gt.env$dt_object <- data.table::rbindlist(
      list(gt.env$dt_object, data.table::as.data.table(obj2)),
      use.names = TRUE
    )

    compute_score(object = 1:2, locations = c("US", "CN", "JP"))
  })
  invisible(NULL)
}
