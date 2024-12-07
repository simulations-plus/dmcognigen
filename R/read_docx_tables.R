#' Read tables from a Word file (docx)
#'
#' @param path the path to a Word file (docx).
#' @param docx_header_pattern one or more patterns of required table header
#'   names. Can be \code{character} or a list containing any combination of
#'   \code{character}, \code{\link[stringr]{regex}}, and
#'   \code{\link[stringr]{fixed}} patterns. For case-insensitive, use regex.
#' @param combine \code{logical} indicating whether to combine the tables into
#'   one data frame or return a list of tables.
#' @param ... optional arguments passed to \code{\link[docxtractr]{read_docx}}.
#'
#' @return a data frame when \code{combined = TRUE}; a list of data frames when
#'   \code{combined = FALSE}.
#' @export
#' 
#' @seealso 
#' \code{\link[docxtractr]{read_docx}}, 
#' \code{\link[docxtractr]{docx_extract_all_tbls}}
#'
#' @examples
#' \dontrun{
#' 
#' # read and combine all tables from a docx.
#' read_docx_tables("results.docx")
#' 
#' # read and combine tables from a docx that match a pattern.
#' read_docx_tables(
#'   path = "requirements.docx",
#'   docx_header_pattern = list(
#'     stringr::regex("variable", ignore_case = TRUE)
#'   )
#' )
#' 
#' # optionally, read in with tracked changes accepted or rejected.
#' read_docx_tables(
#'   path = "requirements.docx",
#'   track_changes = "accept"
#' )
#' }
#' 
read_docx_tables <- function(path, docx_header_pattern = NULL, combine = TRUE, ...) {
  
  assertthat::assert_that(
    is.character(path),
    file.exists(path),
    tolower(fs::path_ext(path)) == "docx",
    msg = "`path` must be the path to a single docx file."
  )
  
  docx <- docxtractr::read_docx(path, ...)
  
  tbls <- docxtractr::docx_extract_all_tbls(docx)
  
  cli::cli_alert_info("{length(tbls)} tables detected in docx file.")
  
  # assign column names as next row until they aren't all V1, V2, ...
  tbls <- tbls %>% 
    purrr::map(function(.x) {
      while(all(startsWith(names(.x), "V")) && nrow(.x) > 1) {
        .x <- docxtractr::assign_colnames(.x, row = 1)
      }
      .x
    })
  
  # subset to matching patterns if provided.
  if(!is.null(docx_header_pattern)) {
    
    if(inherits(docx_header_pattern, c("fixed", "stringr_fixed", "regex", "stringr_regex"))) {
      docx_header_pattern <- list(docx_header_pattern)
    } else if(is.character(docx_header_pattern)) {
      docx_header_pattern <- as.list(docx_header_pattern)
    }
    
    if(!is.list(docx_header_pattern)) {
      cli::cli_abort(
        "{.arg docx_header_pattern} should be a list of: character, {.fn stringr::regex}, or {.fn stringr::fixed} patterns."
      )
    }
    
    # keep tables with matching header names for all provided patterns.
    tbls <- tbls %>% 
      purrr::keep(function(.tbl) {
        all(purrr::map_lgl(docx_header_pattern, function(.pattern) {
          isTRUE(any(stringr::str_detect(names(.tbl), .pattern)))
        }))
      })
    
    cli::cli_alert_info("{length(tbls)} tables with matching header pattern(s).")
    
  }
  
  # combine
  if(isTRUE(combine)) {
    dplyr::bind_rows(tbls)
  } else {
    tbls
  }
  
}
