# globaltrends v.0.0.8
* Create `export_xxx` method for type `list`
* Check whether "db/globaltrends_db.sqlite" file exists in working directory
* Inclusion of "workaround" plot functions that set class `xxx` automatically:
	* `plot_xxx_box`
	* `plot_xxx_bar`
	* `plot_xxx_ts`

# globaltrends v.0.0.7
* Change waiting times for errors
	* Status Code != 200/Limit exceeded -> 60 seconds wait
	* Status Code == 500 -> 1 second wait

# globaltrends v.0.0.6
* To distinguish them from the actual database tables, names of the example data
  object were adapted. Documentation is still available for the respective database
  table.
	* `batch_keywords` -> `example_keywords`
	* `batch_time` -> `example_time`
	* `data_control` -> `example_control`
	* `data_doi` -> `example_doi`
	* `data_object` -> `example_object`
	* `data_score` -> `example_score`
* Waiting period between downloads reduced from 20-30 seconds to 5-10 seconds.

# globaltrends v.0.0.5
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
