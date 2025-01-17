# dmcognigen 0.1.2

* Added `set_decode_factors()` function to modify or create new factor variables based on `decode_tbls`.
* Added `distinct_stationary_variables()` to subset to one row per group and `stationary_variables()` to collect the names of these variables.
* Updated `set_labels()` to accept data sets as sources to inherit labels from and accept labels for individual variable names.
* Updated example data sets `dmcognigen_cov` and `dmcognigen_conc` to use source data from the {pharmaversesdtm} package.
* Added example data sets `dmcognigen_dose`, `dmcognigen_pk`, and `dmcognigen_pk_requirements`.
* Added support for reading tables from Word files with `read_docx_tables()` and `read_requirements()`.
* Added a method to apply requirements attributes to an existing data frame with `as_requirements()`.
* Fixed bug in `search_environment_data()` where evaluating some objects that are not data frames resulted in an error.
* Updated `search_environment_data()` so each search type is optional.
* Updated `cnt_search_result()` to optionally ignore results by name.

# dmcognigen 0.1.1

* Fixed bug in `read_requirements()` where the file with the oldest date was selected instead of the most recent date. 
* Updated print method for `decode_tbls` so decode tables are consistently arranged.
* Added new functions `search_environment_data()` and `cnt_search_result()` for searching dataset contents, variable names, and variable labels.

# dmcognigen 0.1.0

* Added a `NEWS.md` file to track changes to the package.
