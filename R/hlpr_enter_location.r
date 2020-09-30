#' @title Enter location data into database
#'
#' @keywords internal
#'
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble

.enter_location <- function(globaltrends_db) {
  # create countries ----
  countries <- WDI::WDI_data$country
  countries <- as_tibble(countries)
  countries <- filter(countries, region != "Aggregates")
  countries <- select(countries, location = iso2c)
  countries <- WDI::WDI(country = countries$location, indicator = "NY.GDP.MKTP.KD", start = 2018, end = 2018)
  countries <- bind_rows(countries, tibble(iso2c = "TW", country = "Taiwan", NY.GDP.MKTP.KD = 6.08186e+11, year = 2018))
  countries <- mutate(countries, NY.GDP.MKTP.KD = case_when(is.na(NY.GDP.MKTP.KD) ~ 0, TRUE ~ NY.GDP.MKTP.KD))
  countries <- mutate(countries, gdp_share = NY.GDP.MKTP.KD / sum(NY.GDP.MKTP.KD))
  countries <- arrange(countries, -gdp_share)
  countries <- mutate(countries, gdp_cum_share = cumsum(gdp_share))
  countries <- filter(countries, iso2c %in% unique(gtrendsR::countries$country_code) & gdp_share >= 0.001)
  countries <- select(countries, location = iso2c, name = country)
  countries <- mutate(countries, type = "countries")

  # create us_states ----
  us_states <- gtrendsR::countries
  us_states <- mutate_all(us_states, as.character)
  us_states <- us_states[which(us_states$sub_code == "US-AL")[[1]]:which(us_states$sub_code == "US-DC")[[1]], ]
  us_states <- select(us_states, location = sub_code, name)
  us_states <- mutate(us_states, type = "us_states")

  # upload data ----
  dbWriteTable(conn = globaltrends_db, name = "data_locations", value = bind_rows(countries, us_states), append = TRUE)
  message("Successfully entered data into 'data_locations'.")
}
