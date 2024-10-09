# dmcognigen

# Overview

`dmcognigen` provides functions for data management tasks at the Clinical Pharmacology and Pharmacometrics (CPP) business unit of Simulations Plus, Inc.

Load it with:
```r
library(dmcognigen)
```

# Functionality

## Count Observations and Unique Values by Group

- The function expected to be used most frequently in this package is the `cnt()` function.
- `cnt()` is an extension of `dplyr::count()` intended to count the number of distinct occurrences of variables within some group. For example, we commonly `cnt(.data, STUDYID, n_distinct_vars = USUBJID)` to count the number of records within each STUDYID along with the number of unique subjects (USUBJID) within each STUDYID.

## Calculate Standard Variables

- Use the `calculate_*()` family of functions to apply standard equations.

## Read and Leverage Data Requirements

- Check which data requirement files are available with `available_requirements_table()` and import data requirements with `read_requirements()`.
- Use attributes of requirements to apply characteristics defined in data requirements to a dataset.
  - The `"decode_tbls"` attribute can be utilized within `join_decode_labels()` or `join_decode_levels()`.
  - The `"labels_named_list"` attribute can be utilized within `set_labels()`.

## Interact with Decodes

- Extract decodes from vectors with `extract_decode_tbls()`.
- Extract decodes from a dataset with `extract_decode_tbls_from_data()`.
- Merge decodes with `join_decode_labels()` or `join_decode_levels()`.

## Search Datasets

- Find datasets where variables exist with `in_which()`.
- Search for patterns in variable names, variable labels, and variable content across all datasets in an environment with `search_environment_data()` then summarize the results with `cnt_searched_content()`.
