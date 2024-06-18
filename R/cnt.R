#' Count observations and unique values by group
#' 
#' @description 
#' This function is an extension of \code{dplyr::count}. Counts are produced for
#' unique combinations of grouping variables; optionally includes proportions,
#' percentages, and cumulative counts. If any \code{n_distinct_vars} are
#' provided, counts of the unique values are produced for each.
#'
#' @param .data data frame
#' @param ... variables to group by
#' @param n_distinct_vars variables to count the unique values of
#' @param n_distinct_combined logical indicating whether to count the unique
#'   values of the combinations of \code{n_distinct_vars}
#' @param n_cumulative logical indicating whether to include a cumulative sum
#'   variable named "n_cumulative"
#' @param prop logical indicating whether to include a proportion variable named
#'   "prop"
#' @param pct logical indicating whether to include a percentage variable named
#'   "pct"
#'
#' @return data frame
#' @export
#'
#' @examples
#' # count unique values of `am` along with the number of distinct values of
#' # `carb`, `cyl`, and their combination.
#' mtcars %>% 
#'   cnt(am, n_distinct_vars = c(carb, cyl))
cnt <- function(
    .data, 
    ..., 
    n_distinct_vars = NULL, 
    n_distinct_combined = TRUE, 
    n_cumulative = TRUE, 
    prop = FALSE, 
    pct = FALSE
) {
  
  # set visible bindings
  n <- NULL
  
  assertthat::assert_that(
    is.data.frame(.data),
    is.logical(n_distinct_combined),
    is.logical(n_cumulative),
    is.logical(prop),
    is.logical(pct)
  )
  
  # if more than 1 n_distinct_vars are provided, create a variable that combines
  # records across all n_distinct_vars and count the unique number of
  # combinations
  n_distinct_vars_names <- dplyr::select(.data, {{ n_distinct_vars }}) %>% 
    names()
  
  if(length(n_distinct_vars_names) <= 1 || !isTRUE(n_distinct_combined)) {
    
    n_distinct_vars_all_name <- NULL
    
  } else {
    
    n_distinct_vars_all_name <- paste0(n_distinct_vars_names, collapse = "_")
    
    .data <- .data %>% 
      tidyr::unite(
        col = !!n_distinct_vars_all_name,
        {{ n_distinct_vars }},
        sep = "_",
        remove = FALSE,
        na.rm = FALSE
      )
    
  }
  
  # variables that will be dropped
  drop_vars_names <- c("n_cumulative", "prop", "pct")[!c(n_cumulative, prop, pct)]
  
  .data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      dplyr::across(.cols = c({{ n_distinct_vars }}, dplyr::any_of(n_distinct_vars_all_name)),
                    .fns = dplyr::n_distinct,
                    .names = "n_{col}"),
      n = dplyr::n(),
      prop = n / nrow(.),
      pct = 100 * n / nrow(.),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      n_cumulative = cumsum(n)
    ) %>% 
    dplyr::select(
      -dplyr::any_of(drop_vars_names)
    )
  
}
