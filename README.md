# gtrends_doi
Measuring degree of internationalization based on Google Trends data

- initialize database -> at current working directory
  - create empty database
  - create empty tables
    - upload dummy dataframes with 1 row, correct column names, correct column types
    - delete data from columns
    - set indizes for tables
  - enter example data
    - data_geo -> lst_wdi (with wdi package) and lst_us (from gtrends)
    - batch_terms -> example data (from gtrends docu?)
    - batch_time -> set to "2010-01-01 2019-12-31"
    - dict_obj -> example based on batch_terms
- all code refers to current working directory
