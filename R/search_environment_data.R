
#' Search data in an environment for a pattern
#' 
#' Searches variable names, variable labels, and variable content.
#'
#' @param pattern A pattern to search for. Can be \code{\link[stringr]{regex}},
#'   \code{\link[stringr]{fixed}}, or a single character pattern.
#' @param envir The environment or named list to search within.
#' @param search_variable_names,search_variable_labels,search_variable_content
#'   \code{logical} values indicating whether to search that category.
#'
#' @return a \code{search_result} object.
#' @export
#' 
#' @seealso \code{\link{cnt_search_result}}
#'
#' @examples
#' data(mtcars)
#' search_environment_data("cyl")
#' 
#' data(dmcognigen_cov)
#' study_search <- search_environment_data(
#'   stringr::regex("study", ignore_case = TRUE)
#' )
#' study_search
#' 
#' cnt_search_result(study_search, n_distinct_vars = "SUBJID")
search_environment_data <- function(
    pattern, 
    envir = .GlobalEnv,
    search_variable_names = TRUE, 
    search_variable_labels = TRUE, 
    search_variable_content = TRUE
) {
  
  if(!inherits(pattern, c("fixed", "stringr_fixed", "regex", "stringr_regex"))) {
    if(length(pattern) != 1) {
      cli::cli_abort(
        "{.arg pattern} must be one of: {.fn stringr::regex}, {.fn stringr::fixed}, or a single character pattern."
      )
    }
  }
  
  if(any(
    identical(pattern, ""),
    identical(pattern, stringr::regex("")),
    identical(pattern, stringr::fixed(""))
  )) {
    cli::cli_abort(
      "{.arg pattern} can't be the empty string ({.code \"\"})."
    )
  }
  
  assertthat::assert_that(
    is.logical(search_variable_names),
    is.logical(search_variable_labels),
    is.logical(search_variable_content)
  )
  
  # name elements if needed
  if(!is.environment(envir)) {
    if(is.list(envir)) {
      # ideally, the content of the envir argument as a language object will
      # resolve to the length of the envir list. otherwise, will use indexed names.
      names_for_nameless <- as.character(sys.calls()[[1]]["envir"][[1]])[-1]
      if(length(names_for_nameless) != length(envir)) {
        names_for_nameless <- paste0("x", 1:length(envir))
      }
      
      # name elements that are not named.
      if(is.null(names(envir))) {
        names(envir) <- names_for_nameless
      } else {
        missing_names_lgl <- names(envir) == ""
        names(envir)[missing_names_lgl] <- names_for_nameless[missing_names_lgl]
      }
    }
  }
  
  objs <- objects(envir)
  
  if(length(objs) == 0) {
    cli::cli_alert_info("No matches because there are no objects in {.arg envir}.")
    return(invisible(NULL))
  }
  
  df_objects <- objs %>% 
    purrr::keep(
      function(.name) {
        .x <- try(
          eval(parse(text = .name), envir = envir),
          silent = TRUE
        )
        if(inherits(.x, "try-error")) {
          return(FALSE)
        }
        is.data.frame(.x)
      }
    )
  
  if (length(df_objects) == 0) {
    cli::cli_alert_info("No matches because there are no {.cls data.frame} objects in {.arg envir}.")
    return(invisible(NULL))
  }
  
  if(isTRUE(search_variable_names)) {
    has_x_var_names <- df_objects %>% 
      purrr::set_names() %>% 
      purrr::map(
        .f = function(.x) {
          var_names <- names(eval(parse(text = .x), envir = envir))
          stringr::str_subset(var_names, pattern = pattern)
        }
      ) %>% 
      purrr::discard(~ length(.x) == 0)
  } else {
    has_x_var_names <- list()
  }
  
  if(isTRUE(search_variable_labels)) {
    has_x_var_labels <- df_objects %>% 
      purrr::set_names() %>% 
      purrr::map(
        .f = function(.x) {
          df <- eval(parse(text = .x), envir = envir)
          var_labels <- vapply(
            names(df),
            FUN = function(var_name) {
              lbl <- attr(df[[var_name]], "label")
              if(is.null(lbl)) {
                lbl <- ""
              }
              lbl
            },
            FUN.VALUE = character(1)
          )
          names(var_labels)[stringr::str_which(var_labels, pattern = pattern)]
        }
      ) %>% 
      purrr::discard(~ length(.x) == 0)
  } else {
    has_x_var_labels <- list()
  }
  
  if(isTRUE(search_variable_content)) {
    has_x_var_content <- df_objects %>% 
      purrr::set_names() %>% 
      purrr::map(
        .f = function(df_name) {
          df <- eval(parse(text = df_name), envir = envir) %>% 
            dplyr::mutate(
              dplyr::across(
                .cols = dplyr::where(is.factor),
                .fns = as.character
              )
            ) %>% 
            dplyr::select(dplyr::where(is.character))
          
          df %>% 
            purrr::keep(
              .p = ~ isTRUE(any(stringr::str_detect(.x, pattern = pattern)))
            ) %>% 
            names()
        }
      ) %>% 
      purrr::discard(~ length(.x) == 0)
  } else {
    has_x_var_content <- list()
  }
  
  return_list <- c(
    names(has_x_var_names), 
    names(has_x_var_labels), 
    names(has_x_var_content)
  ) %>% 
    unique() %>% 
    sort() %>% 
    purrr::set_names() %>% 
    purrr::map(
      ~ list(
        var_names = has_x_var_names[[.x]],
        var_labels = has_x_var_labels[[.x]],
        var_content = has_x_var_content[[.x]]
      )
    )
  
  structure(
    return_list, 
    pattern = pattern, 
    envir = envir,
    class = unique(c("search_result", class(return_list)))
  )
  
}


#' Print search results
#'
#' @param x a \code{search_result} object
#' @param ... Passed to \code{print}.
#'
#' @export
print.search_result <- function(x, ...) {
  
  cli::cli_h1("Search results")
  
  if(length(x) == 0) {
    cli::cli_verbatim("No search results.")
  } else {
    lapply(names(x), function(df_name) {
      cli::cli_h2(df_name)
      df_result <- x[[df_name]]
      
      if(!is.null(df_result[["var_names"]])) {
        cli::cli_alert_success(
          "Variable names: {df_result[['var_names']]}"
        )
      }
      if(!is.null(df_result[["var_labels"]])) {
        cli::cli_alert_success(
          "Variable labels: {df_result[['var_labels']]}"
        )
      }
      if(!is.null(df_result[["var_content"]])) {
        cli::cli_alert_success(
          "Variable content: {df_result[['var_content']]}"
        )
      }
      
    })
  }
  
  invisible(x)
}


#' Count search results
#' 
#' When patterns are matched in variable names or labels, no subset is
#' performed. When patterns are only matched in variable content, the
#' data.frames are subset to rows where some variable matches the pattern.
#' 
#' @param search_result a \code{search_result} object obtained from
#'   \code{\link{search_environment_data}}.
#' @param df_names,ignore_df_names optional character vectors of data.frame
#'   names to subset to/exclude.
#' @param extra_vars,ignore_vars optional character vectors of variables to
#'   include/exclude in \code{\link{cnt}} results.
#' @inheritParams cnt
#'
#' @return a named list of \code{\link{cnt}} results.
#' @export
#' 
#' @seealso \code{\link{search_environment_data}}
#'
#' @examples
#' data(dmcognigen_cov)
#' study_search <- search_environment_data(
#'   stringr::regex("study", ignore_case = TRUE)
#' )
#' cnt_search_result(
#'   study_search, 
#'   n_distinct_vars = c("USUBJID", "SUBJID"), 
#'   extra_vars = "ACTARM"
#' )
cnt_search_result <- function(
    search_result, 
    n_distinct_vars = NULL,
    n_distinct_combined = TRUE,
    df_names = NULL, 
    ignore_df_names = NULL, 
    extra_vars = NULL, 
    ignore_vars = NULL
) {
  
  if(!inherits(search_result, "search_result")) {
    cli::cli_abort("{.arg search_result} must be a {.cls search_result} object.")
  }
  
  pattern <- attr(search_result, "pattern")
  envir <- attr(search_result, "envir")
  
  if(!is.null(df_names)) {
    stopifnot(is.character(df_names))
    search_result <- search_result[df_names]
  }
  
  if(!is.null(ignore_df_names)) {
    stopifnot(is.character(ignore_df_names))
    search_result <- search_result[setdiff(names(search_result), ignore_df_names)]
  }
  
  no_unquoted <- function(x, name) {
    tried <- try(is.null(x), silent = TRUE)
    if(inherits(tried, "try-error")) {
      cli::cli_abort("{.arg {name}} must be a character vector in {.fun cnt_search_result}.")
    }
  }
  
  no_unquoted(n_distinct_vars, "n_distinct_vars")
  no_unquoted(extra_vars, "extra_vars")
  no_unquoted(ignore_vars, "ignore_vars")
  
  search_result %>% 
    names() %>%
    purrr::set_names() %>% 
    purrr::map(
      .f = function(df_name) {
        df <- eval(parse(text = df_name), envir = envir)
        
        df <- df %>% 
          dplyr::ungroup() %>% 
          dplyr::select(
            dplyr::any_of(search_result[[df_name]][["var_content"]]),
            dplyr::any_of(search_result[[df_name]][["var_names"]]),
            dplyr::any_of(search_result[[df_name]][["var_labels"]]),
            dplyr::any_of(n_distinct_vars),
            dplyr::any_of(extra_vars)
          ) %>% 
          dplyr::select(
            - dplyr::any_of(ignore_vars)
          )
        
        # ensure n_distinct_vars only includes variables that exist
        n_distinct_vars <- df %>%
          dplyr::select(dplyr::any_of(n_distinct_vars)) %>%
          names()
        
        # only subset to pattern if the pattern was not found in the variable
        # names or labels
        if(
          length(search_result[[df_name]][["var_names"]]) + 
          length(search_result[[df_name]][["var_labels"]]) == 0
        ) {
          df <- df %>% 
            dplyr::filter(dplyr::if_any(
              .cols = dplyr::everything(),
              .fns = ~ stringr::str_detect(.x, pattern = pattern)
            ))
        }
        
        if(length(n_distinct_vars) == 0) {
          df %>% 
            cnt(
              dplyr::across(.cols = dplyr::everything()), 
              n_cumulative = FALSE
            )
        } else {
          df %>% 
            cnt(
              dplyr::across(.cols = -dplyr::any_of(c(
                n_distinct_vars, 
                paste0(n_distinct_vars, collapse = "_")
              ))), 
              n_cumulative = FALSE,
              n_distinct_vars = dplyr::any_of(n_distinct_vars),
              n_distinct_combined = n_distinct_combined
            )
        }
        
      }
    )
  
}
