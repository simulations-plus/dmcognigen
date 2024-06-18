#' Interact with decodes and their coded values
#' 
#' @description This family of functions allows the user to transform name-value
#' pairs of coded values in a variety of ways.
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
#' @name extract_decodes
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
#'   # with special characters (<)
#'   "AGECAT", "0=<65 years\n1=65 to 74 years\n2=75 years or older",                      
#'   # with parentheses in decode
#'   "MDV",    "0=PK or PD measure\n1=Dose or Other(EVID=2)",                             
#'   # with special characters and parentheses
#'   "RFCAT",  "1=Normal (>=90 mL/min)\n2=Mild (60-89 mL/min)\n3=Moderate (30-59 mL/min)",
#'   # item that does not include encoding
#'   "BAD",    "This is not a decode"                                                     
#' )
#' 
#' extract_decode_tbls(possible_decodes$var, possible_decodes$decode)
NULL
#> NULL


#' @rdname extract_decodes
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


#' @rdname extract_decodes
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


# thinking through a function to extract from a dataset
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



decode_tbls_lvl_to_lbl <- function(decode_tbls, lvl_to_lbl = "{var}C") {
  
  if(inherits(decode_tbls, "requirements")) {
    decode_tbls <- attr(decode_tbls, "decode_tbls")
  } else if(inherits(decode_tbls, "decode_tbl")) {
    decode_tbls <- as_decode_tbls(list(decode_tbls))
  }
  
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
      "The following level names are missing from {.arg lvl_to_lbl}: {.val {names(lvl_to_lbl_list)[is.na(lvl_to_lbl_list)]}}."
    )
  }
  
  # evaluate
  result <- evaluate_lvl_to_lbl(lvl_to_lbl_list = lvl_to_lbl_list)
  
  # warn if any result has the same output as its input (lvl=lbl)
  lvl_equals_lbl_lgl <- names(result) == result
  if(any(lvl_equals_lbl_lgl)) {
    cli::cli_warn(
      "The following label names are the same as their level names: {.val {names(result)[lvl_equals_lbl_lgl]}}"
    )
  }
  
  # warn if two results are the same.
  same_result_lgl <- duplicated(result)
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
