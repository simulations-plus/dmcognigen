#' Join decode labels or levels
#'
#' @param .data data frame
#' @param decode_tbls \code{decode_tbls} list as returned by
#'   \code{extract_decode_tbls()} or \code{extract_decode_tbls_from_data()}.
#'   Alternatively, the result of \code{read_requirements()} can be used
#'   directly.
#' @param lvl_to_lbl a named \code{list}, where elements correspond to matching
#'   names in \code{decode_tbls}. One element can be left unnamed to provide the
#'   default transformation. Each element should be either: (1) a
#'   function/formula; (2) a custom glue specification. The default is
#'   \code{"{var}C"}, which means the label variable name is the level variable
#'   name appended with a "C".
#' @param ... additional arguments (currently unused)
#'
#' @return data frame with the same number of rows as \code{.data}. New
#'   variables will be included for matching content in \code{.data} and
#'   \code{decode_tbls}.
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
    decode_tbls <- attr(decode_tbls, "decode_tbls")
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