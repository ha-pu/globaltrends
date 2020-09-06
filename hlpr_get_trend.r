# download trend data from google

.get_trend <- function(geo, term, time = "all") {
  out <- try(gtrendsR::gtrends(keyword = term, geo = geo, time = time, onlyInterest = TRUE))
  while (inherits(out, "try-error")) {
    if (attr(out, "condition")$message == "widget$status_code == 200 is not TRUE") {
      Sys.sleep(3600)
    } else {
      Sys.sleep(60)
    }
    out <- try(gtrendsR::gtrends(keyword = term, geo = geo, time = time, onlyInterest = TRUE))
  }
  if (is.null(out$interest_over_time)) {
    return(NULL)
  } else {
    out <- out$interest_over_time
    out$hits <- as.numeric(str_replace(out$hits, "<1", "0.1"))
    out$date <- as.Date(out$date)
    out <- out[,c("geo", "keyword", "date", "hits")]
    Sys.sleep(runif(1, min = 20, max = 30))
    return(out) 
  }
}