library(DBI)
library(dbplyr)
library(forecast)
library(gtrendsR)
library(ineq)
library(lubridate)
library(RSQLite)
library(tidyverse)
library(WDI)

source("R/run_wrld.r")
source("R/run_score.r")
source("R/run_remove.r")
source("R/run_object.r")
source("R/run_map.r")
source("R/run_init.r")
source("R/run_control.r")
source("R/run_agg.r")
source("R/hlpr_test_empty.r")
source("R/hlpr_reset_date.r")
source("R/hlpr_get_trend.r")
source("R/hlpr_enter_geo.r")
source("R/hlpr_compute_doi.r")
source("R/hlpr_adjust_ts.r")
source("R/gtrends_base.r")
source("R/disconnect.r")
source("R/add_batch.r")
