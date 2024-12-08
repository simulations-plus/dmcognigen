#' Create factor variables based on decodes
#'
#' @inheritParams join_decode_labels
#' @param new_names \code{NULL} to update in-place or a named \code{list}, where
#'   elements correspond to matching names in \code{.data} and
#'   \code{decode_tbls}. One element can be left unnamed to provide the default
#'   transformation. Each element should be either: (1) a function/formula; (2)
#'   a custom glue specification, like \code{"{var}CAT"} to create a new
#'   variable that is the original variable name appended with "CAT" for
#'   categorical. The default is \code{NULL}, to update in-place.
#' @param ... additional arguments (currently unused)
#'
#' @return data frame with the same number of rows as \code{.data}. New/modified
#'   variables will be included for matching content in \code{.data} and
#'   \code{decode_tbls}.
#' @export
#'
#' @examples
#' 
#' # create factors in-place
#' dmcognigen_cov |> 
#'   set_decode_factors(dmcognigen_pk_requirements) |>
#'   cnt(SEXF, RACEN)
#' 
#' # or use a mapping convention to define new names
#' dmcognigen_cov |> 
#'   cnt(SEXF, RACEN) |>
#'   set_decode_factors(dmcognigen_pk_requirements, new_names = c(SEXF = "SEXFCAT"))
#'   
#' dmcognigen_cov |> 
#'   cnt(SEXF, RACEN) |>
#'   set_decode_factors(dmcognigen_pk_requirements, new_names = "{var}CAT")
#' 
set_decode_factors <- function(.data, decode_tbls, new_names = NULL, ...) {
  
  assertthat::assert_that(
    is.data.frame(.data)
  )
  
  decode_tbls <- as_decode_tbls(decode_tbls)
  
  assertthat::assert_that(
    inherits(decode_tbls, "decode_tbls")
  )
  
  if(is.null(new_names)) {
    new_names <- purrr::set_names(names(decode_tbls))
  }
  
  assertthat::assert_that(
    length(new_names) > 0
  )
  
  # the default new_names rule can be set as the only unnamed element of
  # new_names.
  default_new_names <- default_lvl_to_lbl(
    lvl_to_lbl = new_names
  )
  
  # list-ify new_names. 
  # each element of decode_tbls should have a corresponding element in
  # new_names_list.
  new_names_list <- names(decode_tbls) %>% 
    purrr::set_names() %>% 
    purrr::map(function(.x) {
      if(.x %in% names(new_names)) {
        new_names[[.x]]
      } else {
        default_new_names
      }
    })
  
  # evaluate
  new_names_list <- evaluate_lvl_to_lbl(lvl_to_lbl_list = new_names_list)
  new_names_list <- tidyr::replace_na(new_names_list, replace = "")
  
  # only keep shared names
  shared_names <- sort(intersect(
    names(decode_tbls), 
    names(new_names_list[new_names_list != ""])
  ))
  shared_names <- sort(intersect(
    shared_names, 
    names(.data)
  ))
  decode_tbls <- as_decode_tbls(decode_tbls[shared_names])
  new_names_list <- new_names_list[shared_names]
  
  if(any(is.na(new_names_list))) {
    cli::cli_abort(
      "The following names are missing from {.arg new_names} and no default was provided: {.val {names(new_names_list)[is.na(new_names_list)]}}."
    )
  }

  # no action for new names that are the same as original names.
  
  # error if two results are the same (and non-blank).
  same_new_name_lgl <- duplicated(new_names_list[new_names_list != ""])
  if(any(same_new_name_lgl)) {
    replicated_new_names <- unique(new_names_list[same_new_name_lgl])
    replicated_new_names_with_names <- replicated_new_names %>% 
      purrr::set_names() %>%
      purrr::map_chr(~ paste0(.x, ": ", paste0(names(new_names_list)[new_names_list == .x], collapse = ", ")))
    
    cli::cli_abort(
      "The following {.arg new_names} are repeated for multiple variables: {.field {replicated_new_names_with_names}}"
    )
    
  }
  
  # create factor variables
  for(i in seq_along(decode_tbls)) {
    
    .decode_tbl <- decode_tbls[[i]]
    
    assertthat::assert_that(
      inherits(.decode_tbl, "decode_tbl")
    )
    
    var_name <- unique(.decode_tbl$var)
    
    new_var_name <- new_names_list[[i]]
    
    if(!var_name %in% colnames(.data)) {
      # not in the dataset, no need to report.
      next
    }
    
    new_variable_message <- paste(
      ifelse(
        new_var_name %in% colnames(.data),
        "Modified variable {.var {new_var_name}}",
        "Created new variable {.var {new_var_name}}"
      ),
      "as a factor of {.var {var_name}}."
    )
    
    new_var <- try({
      factor(
        .data[[var_name]], 
        levels = .decode_tbl$lvl, 
        labels = .decode_tbl$lbl
      )
    })
    
    if(inherits(new_var, "try-error")) {
      cli::cli_warn(
        c(
          "Creating a factor of {.var {var_name}} resulted in an error. Skipping for {.var {var_name}}.",
          "The captured error is below:",
          utils::capture.output(attr(new_var, "condition"))
        )
      )
    } else {
      .data[[new_var_name]] <- new_var
      cli::cli_alert_success(new_variable_message)
    }
    
  }
  
  .data
  
}
