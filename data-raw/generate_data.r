library(tidyverse)

# lst_dates --------------------------------------------------------------------
lst_dates <- list(seq.Date(from = as.Date("2010-01-01"), to = as.Date("2019-12-31"), by = "month"))
lst_dates[[1]] <- as.character(lst_dates[[1]])
lst_dates[[1]] <- str_sub(lst_dates[[1]], 1, 7)

# trunc_rnorm ------------------------------------------------------------------
trunc_rnorm <- function(n, mean = 0, sd = 1, lwr = -Inf, upr = Inf, nnorm = n) {
  samp <- rnorm(n = nnorm, mean = mean, sd = sd)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= n) {
    return(sample(samp, n))
  } else {
    trunc_rnorm(n = n, mean = mean, sd = sd, lwr = lwr, upr = upr, nnorm = nnorm * 1.1)
  }
}

# example_keywords -------------------------------------------------------------
kw_control <- tibble(
  type = "control",
  batch = 1,
  keyword = c("gmail", "maps", "translate", "wikipedia", "youtube")
)

kw_object <- tibble(
  type = "object",
  batch = 1:4,
  keyword = list(
    c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
    c("bayern munich", "bayern munchen"),
    c("amazon", "apple", "facebook", "google"),
    c("instagram", "microsoft", "netflix", "twitter")
  )
) %>%
  unnest(cols = keyword)

example_keywords <- bind_rows(kw_control, kw_object)
usethis::use_data(example_keywords, overwrite = TRUE)

# example_time -----------------------------------------------------------------
ti_control <- tibble(
  type = "control",
  batch = 1,
  time = "2010-01 2019-12"
)

ti_object <- tibble(
  type = "object",
  batch = 1:4,
  time = "2010-01 2019-12"
)

example_time <- bind_rows(ti_control, ti_object)
usethis::use_data(example_time, overwrite = TRUE)

# example_control --------------------------------------------------------------
stat_control <- read_rds("stat_control.rds")

out <- map(
  seq(nrow(stat_control)),
  ~ with(
    stat_control[.x, ],
    trunc_rnorm(n = 120, mean = mean, sd = sd, lwr = min, upr = max)
  )
)

example_control <- stat_control %>%
  select(location, keyword) %>%
  mutate(
    hits = out,
    date = lst_dates,
    batch = 1L
  ) %>%
  unnest(cols = c(hits, date)) %>%
  mutate(hits = as.integer(hits))

usethis::use_data(example_control, overwrite = TRUE)

# example_object ------------------------------------------------------------------
stat_object <- read_rds("stat_object.rds")

out <- map(
  seq(nrow(stat_object)),
  ~ with(
    stat_object[.x, ],
    trunc_rnorm(n = 120, mean = mean, sd = sd, lwr = min, upr = max)
  )
)

example_object <- stat_object %>%
  select(location, keyword, batch_o) %>%
  mutate(
    hits = out,
    date = lst_dates,
    batch = 1L
  ) %>%
  unnest(cols = c(hits, date)) %>%
  mutate(hits = as.integer(hits))

usethis::use_data(example_object, overwrite = TRUE)

# example_score ----------------------------------------------------------------
stat_score <- read_rds("stat_score.rds")

out <- map(
  seq(nrow(stat_score)),
  ~ with(
    stat_score[.x, ],
    trunc_rnorm(n = 120, mean = mean, sd = sd, lwr = min, upr = max)
  )
)

example_score <- stat_score %>%
  select(location, keyword, type) %>%
  left_join(example_keywords, by = "keyword") %>%
  select(-type.y, type = type.x, batch_o = batch) %>%
  mutate(
    score = out,
    date = lst_dates,
    batch = 1L
  ) %>%
  unnest(cols = c(score, date)) %>%
  pivot_wider(names_from = type, values_from = score)

usethis::use_data(example_score, overwrite = TRUE)

# example_doi ------------------------------------------------------------------
stat_doi <- read_rds("stat_doi.rds")

out <- map(
  seq(nrow(stat_doi)),
  ~ with(
    stat_doi[.x, ],
    trunc_rnorm(n = 120, mean = mean, sd = sd, lwr = min, upr = max)
  )
)

example_doi <- stat_doi %>%
  select(keyword, type, measure) %>%
  left_join(example_keywords, by = "keyword") %>%
  select(-type.y, type = type.x, batch_o = batch) %>%
  mutate(
    doi = out,
    date = lst_dates,
    batch_c = 1L,
    locations = "countries"
  ) %>%
  unnest(cols = c(doi, date)) %>%
  pivot_wider(names_from = measure, values_from = doi)

usethis::use_data(example_doi, overwrite = TRUE)
