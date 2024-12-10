#' Subset to stationary content (one row per group)
#' 
#' @description
#' \code{distinct_stationary_variables()} subsets data to one row per group
#' containing only the variables that are constant in all groups.
#' \code{stationary_variables()} returns the names of these variables.
#' 
#' The main difference compared to \code{\link[dplyr]{distinct}} is that this
#' function keeps all stationary variables, while \code{\link[dplyr]{distinct}}
#' keeps either the variables in \code{...} or all variables.
#' 
#' @param .data data frame
#' @param ... grouping variables to use to determine whether other variables are
#'   stationary. If omitted, will subset to variables that are stationary
#'   throughout the entire data frame. Any existing groups are ignored.
#'
#' @return An object of the same type as \code{.data} with fewer or equal
#'   columns, fewer or equal rows, and no groups.
#' @export
#' 
#' @name stationary_variables
#'
#' @examples
#' library(dplyr)
#' 
#' # already one row per group, no change.
#' identical(
#'   dmcognigen_cov,
#'   dmcognigen_cov |>
#'     distinct_stationary_variables(USUBJID)
#' )
#' 
#' # all constant variables (no groups)
#' dmcognigen_cov |>
#'   distinct_stationary_variables()
#' 
#' # names of all constant variable within groups
#' dmcognigen_cov |>
#'   stationary_variables(RACEN, SEXF)
#' 
#' # magrittr pipe lets us pass data as `.`
#' # so we can modify data in a pipeline and reference the resulting stationary variables.
#' dmcognigen_dose %>%
#'   select(1:10) %>%
#'   cnt(across(stationary_variables(.)), n_distinct_vars = USUBJID)
#' 
#' # or can reference the stationary variables in another data frame.
#' # 
#' # count all stationary dose-related variables
#' dmcognigen_pk %>%
#'   select(STUDYID, USUBJID, any_of(stationary_variables(dmcognigen_dose))) %>%
#'   cnt(across(stationary_variables(., DVID)), n_distinct_vars = USUBJID)
#' 
#' # count all stationary concentration-related variables
#' dmcognigen_pk %>%
#'   select(STUDYID, USUBJID, any_of(stationary_variables(dmcognigen_conc))) %>%
#'   cnt(across(stationary_variables(., DVID)), n_distinct_vars = USUBJID)
NULL
#> NULL

#' @export
#' @name stationary_variables
distinct_stationary_variables <- function(.data, ...) {
  
  stationary_vars <- stationary_variables(.data = .data, ...)
  
   # to safely refer to input without considering groups
  .data <- dplyr::ungroup(.data)
  
  # to confirm attributes are kept
  attrs <- attributes(.data)
  
  result <- .data %>% 
    dplyr::select(dplyr::all_of(stationary_vars)) %>% 
    dplyr::group_by(...) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  removed_cols <- setdiff(names(.data), names(result))
  removed_rows <- nrow(.data) - nrow(result)
  
  if(length(removed_cols) == 0) {
    message(cli::format_inline(
      "distinct_stationary_variables: no variables removed"
    ))
  } else {
    message(cli::format_inline(
      "distinct_stationary_variables: removed {length(removed_cols)} variable{?s} ({cli::ansi_collapse(removed_cols, trunc = 5)})"
    ))
  }
  
  if(removed_rows == 0) {
    message(cli::format_inline(
      "distinct_stationary_variables: no rows removed"
    ))
  } else {
    message(cli::format_inline(
      "distinct_stationary_variables: removed {removed_rows} row{?s}, {nrow(result)} row{?s} remaining"
    ))
  }
  
  lost_attrs <- setdiff(names(attrs), names(attributes(result)))
  
  if(length(lost_attrs) > 0) {
    attributes(result)[lost_attrs] <- attrs[lost_attrs]
  }
  class(result) <- class(.data)
  
  result
  
}

#' @export
#' @name stationary_variables
stationary_variables <- function(.data, ...) {
  
  assertthat::assert_that(
    is.data.frame(.data)
  )
  
  .data %>% 
    dplyr::group_by(...) %>% 
    dplyr::group_map(
      .f = function(x, ...) {
        names(x) %>% 
          purrr::set_names() %>% 
          purrr::keep(function(.x) {
            dplyr::n_distinct(x[[.x]], na.rm = FALSE) == 1
          }) %>% 
          names()
      },
      .keep = TRUE
    ) %>% 
    purrr::reduce(intersect)
  
}
