# Direct unit tests for the internal validation layer in R/check_functions.r.
# These are pure functions; no database or network is involved.
# The check_* aliases live in helper-validation.R.

# ── .check_input() ────────────────────────────────────────────────────────────

test_that(".check_input passes valid input invisibly", {
  expect_invisible(check_input("a", "character"))
  expect_true(check_input("a", "character"))
  expect_true(check_input(1L, "numeric"))
  expect_true(check_input(TRUE, "logical"))
  expect_true(check_input(list(), "list"))
  expect_true(check_input(data.frame(), "data.frame"))
})

test_that(".check_input rejects invalid `type` specifications", {
  expect_error(
    check_input("a", type = 1),
    "Internal error: `type` must be a non-missing character scalar.",
    fixed = TRUE
  )
  expect_error(
    check_input("a", type = c("character", "numeric")),
    "Internal error: `type` must be a non-missing character scalar.",
    fixed = TRUE
  )
  expect_error(
    check_input("a", type = NA_character_),
    "Internal error: `type` must be a non-missing character scalar.",
    fixed = TRUE
  )
})

test_that(".check_input rejects a type without a matching is.*() predicate", {
  expect_error(
    check_input("a", type = "definitely_not_a_type"),
    "Internal error: Unknown type predicate 'is.definitely_not_a_type'."
  )
})

test_that(".check_input reports the offending type in its error", {
  expect_error(
    check_input(1, "character"),
    "must be of type character.\nYou provided an object of type double."
  )
  expect_error(
    check_input(TRUE, "character"),
    "must be of type character.\nYou provided an object of type logical."
  )
})

test_that(".check_input uses the `name` override in error messages", {
  expect_error(
    check_input(1, "character", name = "my_argument"),
    "Error: `my_argument` must be of type character.",
    fixed = TRUE
  )
})

# ── .check_length() ───────────────────────────────────────────────────────────

test_that(".check_length passes vectors within the bound", {
  expect_true(check_length("a", 1))
  expect_true(check_length(1:3, 5))
  expect_true(check_length(character(0), 0))
})

test_that(".check_length rejects invalid `max` specifications", {
  err <- "Internal error: `max` must be a non-negative integer scalar."
  expect_error(check_length("a", max = "1"), err, fixed = TRUE)
  expect_error(check_length("a", max = c(1, 2)), err, fixed = TRUE)
  expect_error(check_length("a", max = NA_real_), err, fixed = TRUE)
  expect_error(check_length("a", max = -1), err, fixed = TRUE)
  expect_error(check_length("a", max = 1.5), err, fixed = TRUE)
})

test_that(".check_length reports the actual length in its error", {
  expect_error(
    check_length(1:5, 1),
    "must have length <= 1.\nYou provided an object of length 5."
  )
  expect_error(
    check_length("a", 0),
    "must have length <= 0.\nYou provided an object of length 1."
  )
})

# ── .check_locations() ────────────────────────────────────────────────────────

test_that(".check_locations accepts a character scalar", {
  expect_true(check_locations("countries"))
})

test_that(".check_locations rejects long or non-character input", {
  expect_error(
    check_locations(c("countries", "us_states")),
    "must have length <= 1.\nYou provided an object of length 2."
  )
  expect_error(
    check_locations(1),
    "must be of type character.\nYou provided an object of type double."
  )
})

# ── .check_batch() ────────────────────────────────────────────────────────────

test_that(".check_batch accepts NULL, integers, and whole-number doubles", {
  expect_true(check_batch(NULL))
  expect_true(check_batch(1L))
  expect_true(check_batch(1))
  expect_true(check_batch(3))
})

test_that(".check_batch rejects non-numeric input with the type in the message", {
  expect_error(
    check_batch("1"),
    "Error: Batch id must be an integer value.\nYou provided an object of type character.",
    fixed = TRUE
  )
  expect_error(
    check_batch(TRUE),
    "Error: Batch id must be an integer value.\nYou provided an object of type logical.",
    fixed = TRUE
  )
})

test_that(".check_batch rejects non-finite values", {
  err <- "Error: Batch id must be a finite integer value."
  expect_error(check_batch(NA_real_), err, fixed = TRUE)
  expect_error(check_batch(Inf), err, fixed = TRUE)
  expect_error(check_batch(NaN), err, fixed = TRUE)
})

test_that(".check_batch rejects fractional values", {
  expect_error(
    check_batch(1.5),
    "Error: Batch id must be an integer value.\nYou provided a non-integer numeric value.",
    fixed = TRUE
  )
})
