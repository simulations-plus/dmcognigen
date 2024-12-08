#' Extract decode tables
#' 
#' @description This family of functions can be used to extract
#'   \code{\link{decode_tbls}} from vectors of variable names and decodes.
#'   Generally, users are expected to use \code{extract_decode_tbls}.
#' 
#' \describe{
#'   \item{\code{extract_decode_tbl}}{Generates a \code{decode_tbl} for one
#'   variable name and set of decodes.}
#'   \item{\code{extract_decode_tbls}}{Generates a \code{decode_tbls} list given
#'   vectors of variable names and decodes. Results with no identified decodes
#'   are dropped.}
#' }
#'
#' @param variable_name A \code{character} vector of variable names.
#' @param decode A \code{character} vector. Items are expected to be separated
#'   by new lines. Name-value pairs should be separated by a single equal sign.
#'
#' @seealso 
#' \code{\link{extract_decode_tbls_from_data}} to extract
#' \code{\link{decode_tbls}} from a dataset;
#' \code{\link{join_decode_labels}} and \code{\link{join_decode_levels}} to
#' merge \code{\link{decode_tbls}} to a dataset.
#' 
#' @name extract_decode_tbls
#'
#' @examples
#' possible_decodes <- tibble::tribble(
#'   ~var,     ~decode,
#'   # compact
#'   "SEXF",   "0=Male\n1=Female",                                                        
#'   # yes/no without single quotes
#'   "LGL",    "Y=Yes\nN=No",                                                             
#'   # yes/no with single quotes
#'   "LGLQ",   "'Y'=Yes\n'N'=No",                                                         
#'   # with spaces and numbers
#'   "AGEPED", "0=Neonate to 1 month\n1=Infant: 1 month to 2 years",                      
#'   # with parentheses in decode
#'   "MDV",    "0=PK or PD measure\n1=Dose or Other(EVID=2)",                             
#'   # with special characters and parentheses
#'   "RFCAT",  "1=Normal (>=90 mL/min)\n2=Mild (60-89 mL/min)\n3=Moderate (30-59 mL/min)",
#'   # item that does not include encoding
#'   "BAD",    "This is not a decode"                                                     
#' )
#' 
#' extract_decode_tbls(possible_decodes$var, possible_decodes$decode)
#' 
#' possible_decodes |> 
#'   with(extract_decode_tbls(var, decode))
#' 
NULL
#> NULL


#' @rdname extract_decode_tbls
#' @export
extract_decode_tbl <- function(variable_name, decode) {
  
  # clean-up function for lhs and rhs elements of decodes
  clean_decodes <- function(x) {
    x <- trimws(x)
    
    # remove trailing punctuation
    x <- stringr::str_remove(x, pattern = "[\\.,;]$")
    
    # remove wrapping quoted content
    wrapped_with_single_quotes <- stringr::str_starts(x, "'") & stringr::str_ends(x, "'")
    wrapped_with_double_quotes <- stringr::str_starts(x, '"') & stringr::str_ends(x, '"')
    
    x <- ifelse(
      wrapped_with_single_quotes | wrapped_with_double_quotes,
      stringr::str_sub(x, start = 2, end = -2),
      x
    )
    
    x
  }
  
  decode_pattern <- "(.+)\\s*=+\\s*(.+)\\R*"
  
  assertthat::assert_that(
    length(variable_name) == 1,
    is.character(variable_name)
  )
  
  assertthat::assert_that(
    length(decode) == 1,
    is.character(decode)
  )
  
  # set visible bindings
  source <- var <- lvl <- lbl <- NULL
  
  extract_table <- tibble::tibble(
    var = variable_name,
    source = stringr::str_split(decode, pattern = "\\R") %>%
      unlist() %>%
      stringr::str_subset(pattern = decode_pattern)
  ) %>%
    dplyr::mutate(
      # lhs (lvl) keeps only prior to the first equal sign
      lvl = stringr::str_remove(source, pattern = "\\=.*"),
      # rhs (lbl) is the remaining characters after the equal sign
      lbl = stringr::str_sub(source, start = nchar(lvl) + 2),
      
      lvl = clean_decodes(lvl),
      lbl = clean_decodes(lbl)
    )
  
  # convert lvl to numeric if all results are numeric
  lvl_is_numeric <- all(!is.na(suppressWarnings({
    as.numeric(extract_table$lvl)
  })))
  
  if(lvl_is_numeric) {
    extract_table$lvl <- as.numeric(extract_table$lvl)
  }
  
  result <- extract_table %>%
    dplyr::select(var, lvl, lbl)
  
  as_decode_tbl(result)
  
}


#' @rdname extract_decode_tbls
#' @export
extract_decode_tbls <- function(variable_name, decode) {
  
  assertthat::assert_that(
    length(variable_name) > 0,
    is.character(variable_name)
  )
  
  assertthat::assert_that(
    length(decode) > 0,
    is.character(decode)
  )
  
  assertthat::assert_that(
    length(variable_name) == length(decode)
  )
  
  result_list <- purrr::map2(
    .x = variable_name,
    .y = decode,
    .f = extract_decode_tbl
  ) %>% 
    purrr::set_names(variable_name)
  
  result_decode_tbls <- as_decode_tbls(result_list) %>% 
    purrr::discard(~ nrow(.x) == 0)
  
  as_decode_tbls(result_decode_tbls)
  
}
