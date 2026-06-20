# Unit tests for the three internal DOI dispersion functions.
# These are pure functions — no database is needed.

# .compute_gini ----------------------------------------------------------------

test_that(".compute_gini returns 1 for a uniform distribution", {
  expect_equal(globaltrends:::.compute_gini(rep(1, 5)), 1)
  expect_equal(globaltrends:::.compute_gini(rep(10, 3)), 1)
})

test_that(".compute_gini returns 1 for a single non-NA element", {
  # n=1: g = (2*x*1/x - 2)/1 = 0, so 1-g = 1
  expect_equal(globaltrends:::.compute_gini(42), 1)
})

test_that(".compute_gini with maximally concentrated mass", {
  # sorted c(0,0,0,0,1), n=5, s=1: g = (2*5/1 - 6)/5 = 4/5 → 1-g = 1/5
  expect_equal(globaltrends:::.compute_gini(c(1, 0, 0, 0, 0)), 0.2)
})

test_that(".compute_gini with unequal two-element vector", {
  # sorted c(1,3), n=2, s=4: g = (2*7/4 - 3)/2 = 0.25 → 1-g = 0.75
  expect_equal(globaltrends:::.compute_gini(c(1, 3)), 0.75)
})

test_that(".compute_gini returns NA for all-NA input", {
  expect_true(is.na(globaltrends:::.compute_gini(c(NA_real_, NA_real_))))
})

test_that(".compute_gini returns 0 for all-zero input", {
  expect_equal(globaltrends:::.compute_gini(c(0, 0, 0)), 0)
})

test_that(".compute_gini ignores NAs in mixed input", {
  # c(1, NA, 1) → treated as c(1, 1), uniform → 1
  expect_equal(globaltrends:::.compute_gini(c(1, NA, 1)), 1)
})

# .compute_hhi -----------------------------------------------------------------

test_that(".compute_hhi returns 1 - 1/n for a uniform distribution", {
  expect_equal(globaltrends:::.compute_hhi(rep(1, 5)), 1 - 1 / 5)
  expect_equal(globaltrends:::.compute_hhi(rep(1, 2)), 0.5)
})

test_that(".compute_hhi returns 0 for a monopoly distribution", {
  # p = c(1,0,0,0,0): sum(p^2) = 1 → 1 - 1 = 0
  expect_equal(globaltrends:::.compute_hhi(c(1, 0, 0, 0, 0)), 0)
})

test_that(".compute_hhi with unequal two-element vector", {
  # c(1,3): p = c(0.25, 0.75), sum(p^2) = 0.625 → 1 - 0.625 = 0.375
  expect_equal(globaltrends:::.compute_hhi(c(1, 3)), 0.375)
})

test_that(".compute_hhi returns NA for all-NA input", {
  expect_true(is.na(globaltrends:::.compute_hhi(c(NA_real_, NA_real_))))
})

test_that(".compute_hhi returns 0 for all-zero input", {
  expect_equal(globaltrends:::.compute_hhi(c(0, 0, 0)), 0)
})

test_that(".compute_hhi ignores NAs in mixed input", {
  expect_equal(globaltrends:::.compute_hhi(c(1, NA, 1)), 0.5)
})

# .compute_entropy -------------------------------------------------------------

test_that(".compute_entropy returns 0 for a uniform distribution", {
  expect_equal(globaltrends:::.compute_entropy(rep(1, 5)), 0, tolerance = 1e-10)
  expect_equal(globaltrends:::.compute_entropy(rep(1, 2)), 0, tolerance = 1e-10)
})

test_that(".compute_entropy returns 0 for a single non-zero element", {
  # n=1 non-zero: H = 0, log(1) = 0 → H - log(n) = 0
  expect_equal(globaltrends:::.compute_entropy(42), 0, tolerance = 1e-10)
  expect_equal(globaltrends:::.compute_entropy(c(5, 0, 0)), 0, tolerance = 1e-10)
})

test_that(".compute_entropy returns NA for all-NA input", {
  expect_true(is.na(globaltrends:::.compute_entropy(c(NA_real_, NA_real_))))
})

test_that(".compute_entropy returns 0 for all-zero input", {
  expect_equal(globaltrends:::.compute_entropy(c(0, 0, 0)), 0)
})

test_that(".compute_entropy is negative for a non-uniform distribution", {
  result <- globaltrends:::.compute_entropy(c(2, 1))
  expect_lt(result, 0)
  # Expected: H(p) - log(2), p = c(2/3, 1/3)
  p <- c(2 / 3, 1 / 3)
  expect_equal(result, -sum(p * log(p)) - log(2), tolerance = 1e-10)
})

test_that(".compute_entropy ignores NAs in mixed input", {
  expect_equal(globaltrends:::.compute_entropy(c(1, NA, 1)), 0, tolerance = 1e-10)
})

test_that(".compute_entropy is <= 0 for any non-degenerate distribution", {
  for (scores in list(c(1, 2, 3), c(10, 1, 1, 1), c(50, 30, 20))) {
    expect_lte(globaltrends:::.compute_entropy(scores), 0)
  }
})
