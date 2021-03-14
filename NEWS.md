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
