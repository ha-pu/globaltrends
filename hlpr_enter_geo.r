# enter data_geo

.enter_geo <- function() {
  # create lst_wdi ----
  lst_wdi <- WDI::WDI_data$country
  lst_wdi <- as.data.frame(lst_wdi, stringsAsFactors = FALSE)
  lst_wdi <- filter(lst_wdi, region != "Aggregates")
  lst_wdi <- select(lst_wdi, geo = iso2c)
  lst_wdi <- WDI::WDI(country = lst_wdi$geo, indicator = "NY.GDP.MKTP.KD", start = 2018, end = 2018)
  lst_wdi <- bind_rows(lst_wdi, tibble(iso2c = "TW", country = "Taiwan", NY.GDP.MKTP.KD = 6.08186e+11, year = 2018))
  lst_wdi <- mutate(lst_wdi, NY.GDP.MKTP.KD = case_when(is.na(NY.GDP.MKTP.KD) ~ 0, TRUE ~ NY.GDP.MKTP.KD))
  lst_wdi <- mutate(lst_wdi, share = NY.GDP.MKTP.KD / sum(NY.GDP.MKTP.KD))
  lst_wdi <- arrange(lst_wdi, -share)
  lst_wdi <- mutate(lst_wdi, cum_share = cumsum(share))
  lst_wdi <- filter(lst_wdi, iso2c %in% unique(gtrendsR::countries$country_code))
  lst_wdi <- select(lst_wdi, geo = iso2c, name = country, share, cum_share)
  lst_wdi <- mutate(lst_wdi, type = "lst_wdi")
  
  # create lst_usa ----
  lst_usa <- gtrendsR::countries
  lst_usa <- mutate_all(lst_usa, as.character)
  lst_usa <- lst_usa[which(lst_usa$sub_code == "US-AL")[[1]]:which(lst_usa$sub_code == "US-DC")[[1]],]
  lst_usa <- select(lst_usa, geo = sub_code, name)
  lst_usa <- mutate(lst_usa, type = "lst_usa")
  
  # upload data ----
  dbWriteTable(conn = gtrends_db, name = "data_geo", value = bind_rows(lst_wdi, lst_usa), append = TRUE)
}
