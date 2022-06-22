<!-- badges: start -->
[![R-CMD-check](https://github.com/ha-pu/globaltrends/workflows/R-CMD-check/badge.svg)](https://github.com/ha-pu/globaltrends/actions)
[![test-coverage](https://github.com/ha-pu/globaltrends/workflows/test-coverage/badge.svg)](https://github.com/ha-pu/globaltrends/actions)
<!-- badges: end -->

# globaltrends

<p align="center">
  <img src="hex-globaltrends/hex-globaltrends.png" width="250" height="250">
</p>

Google offers public access to global search volumes through its search engine through the [Google Trends portal](http://www.google.com/trends). The `globaltrends` package downloads search volumes provided by Google Trends and uses them to measure and analyze the **distribution of search trends across countries or within countries**. `globaltrends` allows researchers and analysts to investigate patterns within these trends, such as **degree of internationalization** of firms and organizations or dissemination of political, social, or technological trends across the globe or within single countries.  

To measure degree of internationalization, `globaltrends` offers a wide array of empirical possibilities. It allows researchers to **compare degree of internationalization for various organizations on a unified scale**. In addition, the time-series nature of Google Trends allows for **historical analysis of internationalization patterns and speed** within organizations.  

The enormous detail of the data opens additional applications in research that are impossible with traditional measures of internationalization. For instance, using `globaltrends` on a subnational level allows researchers to **study proliferation within a country** and, for example, to trace a particular market entry. In addition, `globaltrends` offers applications beyond corporate internationalization, such as **data on global interest in products, persons, events, fads or scandals, even academic authors and papers**. 

`globaltrends` provides user-friendly access to Google Trends. The [package vignette](https://github.com/ha-pu/globaltrends/blob/master/globaltrends_Vignette.pdf) offers additional technical details and a basic tutorial. Please, refer to the [package NEWS](https://github.com/ha-pu/globaltrends/blob/master/NEWS.md) for change log.

````
# install ----------------------------------------------------------------------
devtools::install_github("ha-pu/globaltrends", build_vignettes = TRUE)

# packages ---------------------------------------------------------------------
library(dplyr)
library(globaltrends)

# connect to db ----------------------------------------------------------------
initialize_db()
start_db()

# add new control batch --------------------------------------------------------
new_control <- add_control_keyword(keyword = c("gmail", "map", "translate", "wikipedia", "youtube"))

# add new object batch ---------------------------------------------------------
new_object <- add_object_keyword(keyword = c("manchester united", "real madrid"))

# run control download ---------------------------------------------------------
download_control(control = new_control)
download_control_global(control = new_control)

# run object download ----------------------------------------------------------
download_object(object = new_object)
download_object_global(object = new_object)

# compute search score ---------------------------------------------------------
compute_score(control = new_control, object = new_object)
compute_voi(control = new_control, object = new_object)

# compute degree of internationalization ---------------------------------------
compute_doi(control = new_control, object = new_object)

# export data ------------------------------------------------------------------
out_score <- export_score(keyword = "manchester united")
out_voi <- export_voi(keyword = "manchester united")
out_doi <- export_doi(type = "obs", locations = "countries")

# plot data --------------------------------------------------------------------
plot_bar(data = out_score)
plot_ts(data = out_score)
plot_box(data = out_score)
plot_ts(data = out_voi)
plot_box(data = out_voi)
plot_ts(data = out_doi)
plot_box(data = out_doi)
plot_voi_doi(data_voi = out_voi, data_doi = out_doi)

# get abnormal internationalization --------------------------------------------
abnorm_score <- get_abnorm_hist(data = out_score)
plot_bar(data = abnorm_score)
plot_ts(data = abnorm_score)
plot_box(data = abnorm_score)

abnorm_voi <- get_abnorm_hist(data = out_voi)
plot_ts(data = abnorm_voi)
plot_box(data = abnorm_voi)

abnorm_doi <- get_abnorm_hist(data = out_doi)
plot_ts(data = abnorm_doi)
plot_box(data = abnorm_doi)

# disconnect from db -----------------------------------------------------------
disconnect_db()
````

If you use the `globaltrends` package, please cite it as:
Puhr, H., & Müllner, J. (2021). Let me Google that for you: Capturing globalization using Google Trends (SSRN Working Paper 3969013). Available at [https://ssrn.com/abstract=3969013](https://ssrn.com/abstract=3969013).
