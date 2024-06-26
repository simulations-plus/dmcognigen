#' Extract decode tables from data
#'
#' @inheritParams join_decodes
#'
#' @return \code{\link{decode_tbls}}
#' @export
#'
#' @examples
#' dmcognigen_cov |> 
#'   extract_decode_tbls_from_data()
#'
#' dmcognigen_cov |> 
#'   extract_decode_tbls_from_data(
#'     lvl_to_lbl = c(
#'     # specify mapping from lvl to lbl by name
#'     "RACEN" = "RACE", 
#'     # one unnamed, default mapping can be provided
#'     ~ stringr::str_remove(.x, "CD$"))
#'   )
extract_decode_tbls_from_data <- function(.data, lvl_to_lbl = "{var}C") {
  
  assertthat::assert_that(
    is.data.frame(.data)
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
  # each variable in .data should have a corresponding element in
  # lvl_to_lbl_list (temporarily).
  lvl_to_lbl_list <- colnames(.data) %>% 
    purrr::set_names() %>% 
    purrr::map(function(.x) {
      if(.x %in% names(lvl_to_lbl)) {
        lvl_to_lbl[[.x]]
      } else {
        default_lvl_to_lbl
      }
    })
  
  result_lvl_to_lbl <- evaluate_lvl_to_lbl(lvl_to_lbl_list = lvl_to_lbl_list)
  
  # drop missing results
  result_lvl_to_lbl <- result_lvl_to_lbl[!is.na(result_lvl_to_lbl)]
  
  # drop results where the lbl variable evaluated to the lvl variable
  result_lvl_to_lbl <- result_lvl_to_lbl[names(result_lvl_to_lbl) != result_lvl_to_lbl]
  
  # drop results where either the name or result is not in .data
  result_lvl_to_lbl <- result_lvl_to_lbl[names(result_lvl_to_lbl) %in% colnames(.data)]
  result_lvl_to_lbl <- result_lvl_to_lbl[result_lvl_to_lbl %in% colnames(.data)]
  
  purrr::map(
    .x = names(result_lvl_to_lbl),
    .f = function(.var) {
      .lvl <- .data[[.var]]
      .lbl <- .data[[result_lvl_to_lbl[[.var]]]]
      
      tibble::tibble(
        var = .var,
        lvl = .lvl,
        lbl = .lbl
      ) %>% 
        dplyr::distinct() %>% 
        as_decode_tbl()
    }
  ) %>% 
    as_decode_tbls()
  
}

