# globaltrends 0.0.14.9000

# globaltrends 0.0.14

* Remove dependency on `glue` package, calls of `glue::glue` are replaced with `paste` and `paste0`
* Remove dependency on `ineq` package, calls of `ineq::ineq` are replaced with internal functions
* Use functions from `DBI 1.1.0`: `dbCreateTable` and `dbAppendTable`
* Remove the function `plot_map` from the package:
  * The function proofed too inflexible, only world map was included
  * We point users to the functions from the packages `maps` and `ggmap`
  * Remove dependency on the `maps` package
* Remove reference to non-existent table `data_global` in function `remove_data`

# globaltrends 0.0.13

* Set minimum dependency of `stats` and `utils` to version `3.5.0` in line with minimum `R` dependency
* Store globaltrends logo in the *vignettes* folder
* Add explanation of Google's data preparation methodology to vignette
* Change wait intervals for status responses != 200 and add message about automatic retry of download
	* Status == 500: wait 1 second
	* All other responses: wait 60 seconds
* Add function `vacuum_data` to free unused memory after `remove_data`
* Remove all usage of `.data` to comply with `tidyselect 1.2.0`, this applies to calls of:
	* `dplyr::rename`
	* `dplyr::select`
	* `purrr::map`
	* `purrr::walk`
	* `tidyr::nest`
	* `tidyr::pivot_longer`
	* `tidyr::pivot_wider`
	* `tidyr::unnest`
* As a consequence of the changes in `tidyselect 1.2.0`, several objects are defined as global variables (see `globals.r` for details) 
* Replace `size` with `linewidth` to comply with `ggplot2 3.4.0`
* Add the argument `multiple = "all"` and `multiple = "error"` to comply with `dplyr 1.1.0`
* Facilitate handling of location = "NA - Namibia", the function `add_locations` does automatically drop the location
* Remove dependency on `WDI` package, references to `WDI::WDI_data` are replaced by data objects `countries` and `countries_wdi`

# globaltrends 0.0.12

* Stop direct exports from functions to .GlobalEnv
* Exports of objects (e.g., SQL connections, keyword tables) are redirected to package environment `gt.env`
* Add handle for Namibia ISO code

# globaltrends 0.0.11

* Add function plot_map()
* Add automatic stringr::str_squish for keywords and synonyms
* Add documentation on usage of search topics

# globaltrends 0.0.10

* Change indication and handling of synonyms (synonyms are now ignored in `export_score` and `export_voi`)

# globaltrends 0.0.9

* Fasten computation of search scores for data with synonyms
* Fix bugs for control keywords that have a mean of 0 for their search volume time series

# globaltrends 0.0.8

* Adapt `export_xxx` for `vector` and `list` inputs
* Optimize RAM usage of `export_xxx`
* Check whether "db/globaltrends_db.sqlite" file exists in working directory
* Inclusion of "workaround" plot functions that set class `xxx` automatically:
	* `plot_xxx_box`
	* `plot_xxx_bar`
	* `plot_xxx_ts`

# globaltrends 0.0.7

* Change waiting times for errors
	* Status Code != 200/Limit exceeded -> 60 seconds wait
	* Status Code == 500 -> 1 second wait

# globaltrends 0.0.6

* To distinguish them from the actual database tables, names of the example data
  object were adapted. Documentation is still available for the respective database
  table.
	* `batch_keywords` -> `example_keywords`
	* `batch_time` -> `example_time`
	* `data_control` -> `example_control`
	* `data_doi` -> `example_doi`
	* `data_object` -> `example_object`
	* `data_score` -> `example_score`
* Waiting period between downloads reduced from 20-30 seconds to 5-10 seconds

# globaltrends 0.0.5

* Added classes for output from `export_xxx`
	* `export_score` -> class("exp_score")
	* `export_voi` -> class("exp_voi")
	* `export_doi` -> class("exp_doi")
* `export_xxx` + `get_abnorm_hist` supersedes `export_xxx_change`
	* method for `export_score` -> class("exp_score")
	* method for `export_voi` -> class("exp_voi")
	* method for `export_doi` -> class("exp_doi")
* `plot_bar` supersedes `plot_score`
	* method for `export_score` -> class("exp_score")
	* method for `export_score` -> `get_abnorm_hist` -> class("abnorm_score")
* `plot_ts` supersedes `plot_voi_ts` and `plot_doi_ts`
	* method for `export_score` -> class("exp_score")
	* method for `export_score` -> `get_abnorm_hist` -> class("abnorm_score")
	* method for `export_voi` -> class("exp_voi")
	* method for `export_voi` -> `get_abnorm_hist` -> class("abnorm_voi")
	* method for `export_doi` -> class("exp_doi")
	* method for `export_doi` -> `get_abnorm_hist` -> class("abnorm_doi")
* `plot_box` supersedes `plot_voi_box` and `plot_doi_box`
	* method for `export_score` -> class("exp_score")
	* method for `export_score` -> `get_abnorm_hist` -> class("abnorm_score")
	* method for `export_voi` -> class("exp_voi")
	* method for `export_voi` -> `get_abnorm_hist` -> class("abnorm_voi")
	* method for `export_doi` -> class("exp_doi")
	* method for `export_doi` -> `get_abnorm_hist` -> class("abnorm_doi")
