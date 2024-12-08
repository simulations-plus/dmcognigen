
# decode_tbl class functions ----------------------------------------------

#' Build decode tables
#' 
#' @description Objects of class \code{decode_tbl} include all pairs of
#'   \code{lvl} and \code{lbl} for a single \code{var}. They are generated based
#'   on variable definitions or values within a dataset. 
#' 
#' Objects of \code{decode_tbls} are lists of \code{decode_tbl} objects.
#' 
#' @param x A tibble or data frame for \code{as_decode_tbl()}. A list for
#'   \code{as_decode_tbls()}.
#' 
#' @return a \code{decode_tbl} or \code{decode_tbls} object.
#' 
#' @seealso \code{\link{print.decode_tbl}}, \code{\link{extract_decode_tbls}},
#' \code{\link{extract_decode_tbls_from_data}}, \code{\link{join_decodes}}
#'
#' @name decode_tbls
NULL
#> NULL

#' @keywords internal
decode_tbl <- function(x = tibble::tibble()) {
  
  # set visible bindings
  var <- lvl <- NULL
  
  required_var_names <- c("var", "lvl", "lbl")
  
  empty_decode_tbl <- function() {
    tibble::tibble(
      var = character(),
      lvl = numeric(),
      lbl = character()
    )
  }
  
  if(!is.data.frame(x)) {
    cli::cli_abort(c(
      x = "{.arg x} must be a data frame."
    ))
  }
  
  missing_var_names <- setdiff(required_var_names, colnames(x))
  
  if(length(missing_var_names) == 0) {
    # x already has required content
    xx <- x
  } else if(nrow(x) == 0 && ncol(x) == 0) {
    # x is missing required content but has no content (default case)
    xx <- empty_decode_tbl()
  } else {
    # x is missing required content and has other content
    cli::cli_abort(c(
      x = "Objects of class {.cls decode_tbl} must have columns named {.val {required_var_names}}.",
      i = "These columns are missing from {.arg x}: {.val {missing_var_names}}."
    ))
  }
  
  # retain original class of x
  structure(dplyr::arrange(xx, var, lvl), class = unique(c("decode_tbl", class(x))))
}


#' @rdname decode_tbls
#' @export
as_decode_tbl <- function(x = tibble::tibble()) {
  validate_decode_tbl(decode_tbl(x))
}

# Checks a data frame for the requirements of a decode_tbl
validate_decode_tbl <- function(x = tibble::tibble()) {
  
  # set visible bindings
  var <- lvl <- n <- lbl <- NULL
  
  # confirm bare-minimum structure of decode_tbl is provided
  x <- decode_tbl(x)
  
  # confirm single lbl per lvl (aka multiple of the same lvl)
  dup_decode_test_lbl <- x %>% 
    dplyr::group_by(var, lvl) %>% 
    dplyr::summarise(n = dplyr::n(), .groups = "keep") %>% 
    dplyr::ungroup()
  
  if(any(dup_decode_test_lbl$n > 1)) {
    
    dup_vars <- dup_decode_test_lbl %>% 
      dplyr::filter(n > 1) %>% 
      dplyr::pull(var) %>% 
      unique()
    
    cli::cli_abort(c(
      x = "The following variables include duplicate decode levels: {.val {dup_vars}}"
    ))
    
  }
  
  # confirm single lvl per lbl
  dup_decode_test_lvl <- x %>% 
    dplyr::group_by(var, lbl) %>% 
    dplyr::summarise(n = dplyr::n_distinct(lvl), .groups = "keep") %>% 
    dplyr::ungroup()
  
  if(any(dup_decode_test_lvl$n > 1)) {
    
    dup_vars <- dup_decode_test_lvl %>% 
      dplyr::filter(n > 1) %>% 
      dplyr::pull(var) %>% 
      unique()
    
    cli::cli_warn(c(
      x = "The following variables include duplicate decode labels: {.val {dup_vars}}"
    ))
    
  }
  
  x
}


#' @rdname decode_tbls
#' @export
as_decode_tbls <- function(x = list()) {
  
  if(inherits(x, "decode_tbl")) {
    x <- list(x)
  } else if(!is.null(attr(x, "decode_tbls"))) {
    x <- attr(x, "decode_tbls")
  }
  
  if(!is.list(x)) {
    cli::cli_abort(c("x" = "{.arg x} must be a list."))
  }
  
  if(length(x) == 0) {
    xx <- structure(list(), class = "decode_tbls")
    return(xx)
  }
  
  # confirm that each element of x is a decode_tbl, or can maybe be coerced to
  # one
  x_classes_lgl <- purrr::map_lgl(x, inherits, what = c("decode_tbl", "data.frame"))
  
  if(any(!x_classes_lgl)) {
    cli::cli_abort(
      x = "All elements of {.arg x} must be coercible to a {.cls decode_tbl}"
    )
  }
  
  # try to convert each element of x to a decode_tbl.
  xx <- purrr::map(x, as_decode_tbl)
  
  # confirm each var is included in only one decode_tbl
  all_vars <- unique(unlist(purrr::map(xx, ~ .x$var)))
  
  dup_vars <- all_vars %>% 
    purrr::keep(function(.var) {
      xx_has_var_lgl <- purrr::map_lgl(xx, function(.xx) {
        .var %in% .xx$var
      })
      sum(xx_has_var_lgl) > 1
    })
  
  if(length(dup_vars) > 0) {
    cli::cli_abort(c(
      x = "The following decode variables are duplicated: {.val {dup_vars}}"
    ))
  }
  
  # confirm names are set as the single vars within each item
  names(xx)[purrr::map_lgl(xx, ~ nrow(.x) > 0)] <- all_vars
  
  structure(xx, class = unique(c("decode_tbls", class(xx))))
  
}


# print methods -----------------------------------------------------------

#' Print values of decode tables
#'
#' @param x A \code{decode_tbl} or \code{decode_tbls} object.
#' @param align A string for the alignment of the output. Either "left"
#'   (default), "center" (aligns '='), or "right".
#' @param ... Passed to \code{print}.
#' 
#' @name print.decode_tbls
#' 
#' @export
print.decode_tbl <- function(x, align = c("left", "center", "right"), ...) {
  
  # set visible bindings
  var <- lvl <- lbl <- fmt <- dcd_center <- dcd_left <- dcd_right <- diff_nchar <- num_char <- max_nchar <- max_nchar_tot <- NULL
  
  align <- match.arg(align)
  
  if(nrow(x) == 0) {
    return(invisible(x))
  }
  
  x_format <- x %>% 
    dplyr::group_by(var) %>% 
    dplyr::mutate(
      dcd_left = paste0(lvl,"=",lbl),
      num_char = nchar(dcd_left),
      max_nchar_tot = max(num_char),
      diff_nchar = max_nchar_tot - num_char,
      dcd_right = paste0(stringr::str_pad(
        paste0(lvl, "=", lbl), 
        width = num_char + diff_nchar, 
        side = "left", 
        pad = " "
      )),
      max_nchar = max(nchar(lvl)),
      dcd_center = paste0(stringr::str_pad(
        lvl, 
        width = max_nchar, 
        side = "left", 
        pad = " "
      ), "=", lbl),
      
      fmt = list(
        left = dcd_left,
        center = dcd_center,
        right = dcd_right
      )[[align]]
      
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(var, lvl)
  
  x_format_list <- x_format %>% 
    split(~ var)
  
  lapply(names(x_format_list), function(.var) {
    
    .x_format <- x_format_list[[.var]]
    
    # intended to work nicely when called from other functions like
    # print.decode_tbls
    cli::cli_h2(.var)
    cli::cli_verbatim(.x_format$fmt)
    
  })
  
  invisible(x)
  
}

#' @rdname print.decode_tbls
#' @export
print.decode_tbls <- function(x, align = c("left", "center", "right"), ...) {
  
  if(length(x) == 0) {
    return(invisible(x))
  }
  
  cli::cli_h1("Decode tables")
  lapply(x, print)
  
  invisible(x)
  
}
