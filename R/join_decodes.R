#' Join decode labels or levels
#'
#' @param .data data frame
#' @param decode_tbls \code{decode_tbls} list as returned by
#'   \code{extract_decode_tbls()} or \code{extract_decode_tbls_from_data()}.
#'   Alternatively, the result of \code{read_requirements()} can be used
#'   directly.
#' @param lvl_to_lbl a named \code{list}, where elements correspond to matching
#'   names in \code{.data} and \code{decode_tbls}. One element can be left
#'   unnamed to provide the default transformation. Each element should be
#'   either: (1) a function/formula; (2) a custom glue specification. The
#'   default is \code{"{var}C"}, which means the label variable name is the
#'   level variable name appended with a "C".
#' @param ... additional arguments (currently unused)
#'
#' @return data frame with the same number of rows as \code{.data}. New
#'   variables will be included for matching content in \code{.data} and
#'   \code{decode_tbls}.
#'   
#' @seealso 
#' \code{\link{extract_decode_tbls_from_data}} to extract decode tables from a
#' dataset;
#' \code{\link{extract_decode_tbls}} to extract decode tables from a table
#' describing variable decodes.
#' 
#' @name join_decodes
NULL
#> NULL

#' @rdname join_decodes
#' @export
join_decode_labels <- function(.data, decode_tbls, lvl_to_lbl = "{var}C", ...) {
  
  .join_decodes(
    .data = .data,
    decode_tbls = decode_tbls,
    lvl_to_lbl = lvl_to_lbl,
    join = "lbl",
    ...
  )
  
}

#' @rdname join_decodes
#' @export
join_decode_levels <- function(.data, decode_tbls, lvl_to_lbl = "{var}C", ...) {
  
  .join_decodes(
    .data = .data,
    decode_tbls = decode_tbls,
    lvl_to_lbl = lvl_to_lbl,
    join = "lvl",
    ...
  )
  
}

# helper function for both join_decode_labels and join_decode_levels
.join_decodes <- function(.data, decode_tbls, lvl_to_lbl = "var{C}", join = c("lbl", "lvl"), ...) {
  
  join <- match.arg(join)
  
  # set visible bindings
  var <- lvl <- lbl <- NULL
  
  assertthat::assert_that(
    is.data.frame(.data)
  )
  
  if(inherits(decode_tbls, "requirements")) {
    decode_tbls <- as_decode_tbls(decode_tbls)
  } else if(inherits(decode_tbls, "decode_tbl")) {
    decode_tbls <- as_decode_tbls(list(decode_tbls))
  }
  
  assertthat::assert_that(
    inherits(decode_tbls, "decode_tbls")
  )
  
  # suppressing the warnings because we will drop the unmatched elements.
  lvl_to_lbl <- suppressWarnings({
    decode_tbls_lvl_to_lbl(
      decode_tbls = decode_tbls,
      lvl_to_lbl = lvl_to_lbl
    )
  })
  
  # only keep shared names
  shared_names <- sort(intersect(
    names(decode_tbls), 
    names(lvl_to_lbl[lvl_to_lbl != ""])
  ))
  decode_tbls <- as_decode_tbls(decode_tbls[shared_names])
  lvl_to_lbl <- lvl_to_lbl[shared_names]
  
  for(i in seq_along(decode_tbls)) {
    
    .decode_tbl <- decode_tbls[[i]]
    
    assertthat::assert_that(
      inherits(.decode_tbl, "decode_tbl")
    )
    
    lvl_name <- unique(.decode_tbl$var)
    
    lbl_name <- lvl_to_lbl[[i]]
    
    x_name <- ifelse(join == "lbl", lvl_name, lbl_name)
    y_name <- ifelse(join == "lbl", lbl_name, lvl_name)
    
    # when joining labels, confirm level exists and label does not exist.
    # when joining levels, confirm label exists and level does not exist.
    if(!x_name %in% colnames(.data)) {
      cli::cli_warn(
        "{.var {x_name}} does not exist in {.arg .data}. Skipping join for {.var {x_name}}."
      )
      next
    }
    
    if(y_name %in% colnames(.data)) {
      cli::cli_warn(
        "{.var {y_name}} already exists in {.arg .data}. Skipping join for {.var {x_name}}."
      )
      next
    }
    
    decode_data <- tibble::tibble(
      lvl_name = .decode_tbl$lvl,
      lbl_name = .decode_tbl$lbl
    )
    names(decode_data) <- c(lvl_name, lbl_name)
    
    joined_data <- try({
      dplyr::left_join(
        x = .data,
        y = decode_data,
        by = x_name
      )
    })
    
    if(inherits(joined_data, "try-error")) {
      cli::cli_warn(
        c(
          "Joining {.var {y_name}} by {.var {x_name}} resulted in an error. Skipping join for {.var {x_name}}.",
          "The captured error is below:",
          utils::capture.output(attr(joined_data, "condition"))
        )
      )
    } else if(y_name %in% colnames(joined_data)) {
      cli::cli_alert_success("Joined {.var {y_name}} by {.var {x_name}}.")
      
      if(any(is.na(joined_data[[y_name]]))) {
        missing_values_where <- sort(unique(joined_data[is.na(joined_data[[y_name]]), ][[x_name]]))
        cli::cli_warn(
          "Missing values for {.var {y_name}} where {.var {x_name}} is: {.val {missing_values_where}}"
        )
      }
      
      joined_data_decode_tbl <- extract_decode_tbls_from_data(
        joined_data,
        lvl_to_lbl = purrr::set_names(y_name, x_name)
      )
      
      print(joined_data_decode_tbl[[1]])
      
      .data <- joined_data
    }
    
  }
  
  .data
  
}


#' @rdname join_decodes
#' @export
decode_tbls_lvl_to_lbl <- function(decode_tbls, lvl_to_lbl = "{var}C") {
  
  decode_tbls <- as_decode_tbls(decode_tbls)
  
  assertthat::assert_that(
    inherits(decode_tbls, "decode_tbls")
  )
  
  assertthat::assert_that(
    length(lvl_to_lbl) > 0
  )
  
  # the default lvl_to_lbl rule can be set as the only unnamed element of
  # lvl_to_lbl.
  default_lvl_to_lbl <- default_lvl_to_lbl(
    lvl_to_lbl = lvl_to_lbl
  )
  
  # list-ify lvl_to_lbl. 
  # each element of decode_tbls should have a corresponding element in
  # lvl_to_lbl_list.
  lvl_to_lbl_list <- names(decode_tbls) %>% 
    purrr::set_names() %>% 
    purrr::map(function(.x) {
      if(.x %in% names(lvl_to_lbl)) {
        lvl_to_lbl[[.x]]
      } else {
        default_lvl_to_lbl
      }
    })
  
  if(any(is.na(lvl_to_lbl_list))) {
    cli::cli_warn(
      "The following level names are missing from {.arg lvl_to_lbl} and no default was provided: {.val {names(lvl_to_lbl_list)[is.na(lvl_to_lbl_list)]}}."
    )
  }
  
  # evaluate
  result <- evaluate_lvl_to_lbl(lvl_to_lbl_list = lvl_to_lbl_list)
  result <- tidyr::replace_na(result, replace = "")
  
  # warn if any result has the same output as its input (lvl=lbl)
  lvl_equals_lbl_lgl <- names(result) == result
  if(any(lvl_equals_lbl_lgl)) {
    cli::cli_warn(
      "The following label names are the same as their level names: {.val {names(result)[lvl_equals_lbl_lgl]}}"
    )
  }
  
  # warn if two results are the same (and non-blank).
  same_result_lgl <- duplicated(result[result != ""])
  if(any(same_result_lgl)) {
    replicated_results <- unique(result[same_result_lgl])
    replicated_results_with_names <- replicated_results %>% 
      purrr::set_names() %>%
      purrr::map_chr(~ paste0(.x, ": ", paste0(names(result)[result == .x], collapse = ", ")))
    
    cli::cli_warn(
      "The following label names are repeated for multiple level names: {.field {replicated_results_with_names}}"
    )
    
  }
  
  result
  
}


# helper function to create the default lvl_to_lbl
default_lvl_to_lbl <- function(lvl_to_lbl = "{var}C") {
  
  if(inherits(lvl_to_lbl, "formula")) {
    lvl_to_lbl
  } else if(length(lvl_to_lbl) == 1) {
    if(!is.null(names(lvl_to_lbl))) {
      # when a single named element is provided, set no default.
      NA_character_
    } else if(is.list(lvl_to_lbl)) {
      # when a single unnamed element is provided, set that as the default.
      lvl_to_lbl[[1]]
    } else {
      lvl_to_lbl
    }
  } else if(is.null(names(lvl_to_lbl))) {
    cli::cli_abort(
      "Only unnamed elements of {.arg lvl_to_lbl} detected. A maximum of 1 unnamed element can be provided as the default {.arg lvl_to_lbl}."
    )
  } else {
    # attempt to detect a single default
    empty_names_lgl <- names(lvl_to_lbl) == ""
    if(sum(empty_names_lgl) > 1) {
      cli::cli_abort(
        "Multiple unnamed elements of {.arg lvl_to_lbl} detected. A maximum of 1 unnamed element can be provided as the default {.arg lvl_to_lbl}."
      )
    } else if(sum(empty_names_lgl) == 1) {
      lvl_to_lbl[[which(empty_names_lgl)]]
    } else {
      # all names are already set.
      NA_character_
    }
  }
  
}


# helper function to evaluate functions, formulas, and glue for lvl_to_lbl.
evaluate_lvl_to_lbl <- function(lvl_to_lbl_list) {
  
  lvl_to_lbl_list %>% 
    names() %>% 
    purrr::set_names() %>% 
    purrr::map_chr(
      .f = function(.name) {
        # lvl_to_lbl can take many forms.
        
        .lvl_to_lbl <- lvl_to_lbl_list[[.name]]
        
        if(inherits(.lvl_to_lbl, "function")) {
          # function: apply the function with the decode_tbl name as input.
          do.call(.lvl_to_lbl, args = list(.name))
        } else if(inherits(.lvl_to_lbl, "formula")) {
          # formula: convert to function then apply with decode_tbl name as input.
          do.call(
            what = rlang::as_function(.lvl_to_lbl),
            args = list(.name)
          )
        } else if(is.na(.lvl_to_lbl)) {
          .lvl_to_lbl
        } else if(inherits(.lvl_to_lbl, "character")) {
          # character: treat as glue specification.
          with(
            list(var = .name),
            as.character(glue::glue(.lvl_to_lbl))
          )
        } else {
          cli::cli_abort(
            "No method defined in {.fun evaluate_tbls_lvl_to_lbl} for class {.class {class(.lvl_to_lbl)}}"
          )
        }
      }
    )
  
}
