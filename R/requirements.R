#' Get available and latest data requirements files
#' 
#' @description This family of functions allows the user to explore, import, and
#'   leverage the contents of data requirements files. Supported files types are
#'   Excel files (xlsx, xls, xlsm) and Word files (docx only). Note that \code{sheets
#'   = NULL} must be used to include docx files in searches.
#'
#' \describe{
#'   \item{\code{read_requirements}}{Read the latest data requirements file.}
#'   \item{\code{as_requirements}}{Apply requirements attributes to a data frame.}
#'   \item{\code{available_requirements_table}}{Get available data requirements
#'   files. Returns a \code{tibble} including available data requirements paths
#'   and other information.}
#' }
#'
#' @param path a single directory path or the path to a data requirements file.
#'   For \code{read_requirements}, providing a directory path will result in the
#'   latest matching data requirements file being selected, while providing a
#'   file path will result in that file being selected. Defaults to the working
#'   directory.
#' @param pattern \code{character} string containing a regular expression. Only
#'   file names which match the regular expression will be returned. Defaults to
#'   \code{"req"}.
#' @param sheet either a \code{character} vector of required Excel sheet
#'   name(s), the \code{numeric} index of the sheet position, or \code{NULL}
#'   (for no required sheet names, and to include docx files). Only one sheet
#'   name or index should be provided to \code{read_requirements}. Defaults to
#'   \code{"specs"}, so `sheet = NULL` must be used to match the latest docx
#'   file.
#' @inheritParams read_docx_tables
#' @param date_format \code{character} indicating the format of the date.
#'   Defaults to the year-month-day format \code{"ymd"}.
#' @param subset an expression that returns a logical value and is defined in
#'   the terms of the imported requirements table (like
#'   \code{\link[dplyr]{filter}}). If the expression results in an error, that
#'   error is reported as a warning and the subset is not applied. An example is
#'   \code{subset = pk_ard == "x"}, which indicates to subset to variables
#'   marked for inclusion in the PK Analysis Ready Dataset.
#' @param drop_qc \code{logical} indicating whether to remove versions of the
#'   data requirements that are used for QC. These are identified by patterns of
#'   "qc" or "marked" in the filename.
#' @param variable_name_col,variable_label_col,decode_col \code{character}
#'   column names in the data requirements that describe the variable names,
#'   their labels, and their decodes. These should match results after
#'   transformations performed by \code{make_clean_names_fn}.
#' @param make_clean_names_fn a function/formula that cleans/transforms the
#'   original variable names. Defaults to
#'   \code{\link[janitor]{make_clean_names}}.
#' @param ... optional arguments passed to either
#'   \code{\link[openxlsx]{read.xlsx}} or \code{\link[docxtractr]{read_docx}}.
#' 
#' @name requirements
#' 
#' @examples
#' \dontrun{
#' # read the latest requirements file in the working directory, based on CPP defaults
#' reqs <- read_requirements()
#' 
#' # specify a particular file and sheet
#' reqs <- read_requirements(path = "requirements.xlsx", sheet = 1)
#' 
#' # read the latest docx requirements file
#' reqs <- read_requirements(
#'   pattern = "req.*docx",
#'   sheet = NULL, 
#'   docx_header_pattern = stringr::regex("variable", ignore_case = TRUE)
#' )
#' 
#' # apply attributes to an existing data frame
#' reqs <- as_requirements(reqs_df)
#' 
#' # get all available requirements files
#' available_requirements_table()
#' 
#' # include docx files in search
#' available_requirements_table(sheet = NULL)
#' 
#' # only include requirements with a specs sheet
#' available_requirements_table(sheet = "specs")
#' 
#' # include QC versions
#' available_requirements_table(drop_qc = FALSE)
#' }
#' 
NULL
#> NULL


#' @rdname requirements
#' @importFrom rlang %||%
#' @export
read_requirements <- function(
    path = ".",
    pattern = "req",
    sheet = "specs",
    docx_header_pattern = NULL,
    date_format = c("ymd", "mdy", "dmy"),
    subset = NULL,
    variable_name_col = "variable_name",
    variable_label_col = "variable_label",
    decode_col = "format_decode",
    make_clean_names_fn = janitor::make_clean_names,
    ...
  ) {
  
  assertthat::assert_that(
    length(path) == 1,
    is.character(path),
    file.exists(path),
    msg = "`path` should be a single existing file or directory."
  )
  
  if(!is.null(sheet)) {
    assertthat::assert_that(
      length(sheet) == 1,
      is.character(sheet) || is.numeric(sheet),
      msg = "`sheet` should be a single sheet name, numeric index, or NULL."
    )
  }
  
  # if a directory was provided, select the appropriate file.
  # this will update path from a directory to the latest requirements file.
  if(dir.exists(path)) {
    
    # set visible bindings
    modified <- date <- NULL
    
    if(!is.null(docx_header_pattern) && !is.null(sheet)) {
      cli::cli_alert_warning(c(
        "Both {.arg docx_header_pattern} and {.arg sheet} are defined as not NULL. ",
        "{.code sheet = NULL} is required to search for docx tables."
      ))
    }
    
    requirements_table <- available_requirements_table(
      path = path,
      pattern = pattern,
      sheet = sheet,
      date_format = date_format
    )
    
    # available_requirements_table() will produce a warning in this case
    if(nrow(requirements_table) == 0) {
      return(invisible(tibble::tibble()))
    }
    
    # set path to latest file. 
    # if available, the max date in the filename is preferred. then max modified time.
    if(all(is.na(requirements_table$path_date))) {
      cli::cli_warn("No dates were detected in filenames matching {.arg pattern}. Using modified date to select latest file.")
      path <- requirements_table %>% 
        dplyr::slice_max(modified, with_ties = FALSE) %>%
        dplyr::pull(path)
    } else {
      path <- requirements_table %>% 
        dplyr::arrange(dplyr::desc(date), dplyr::desc(modified), path) %>%
        dplyr::slice(1) %>% 
        dplyr::pull(path)
    }
    
    # report information about detected file
    cli::cli_alert_success("Detected requirements file: {.file {basename(path)}}")
    
  } else {
    cli::cli_alert_success("Requirements file: {.file {basename(path)}}")
  }
  
  cli::cli_alert_info("Modification time: {file.mtime(path)}")
  
  path_is_docx <- identical(tolower(fs::path_ext(path)), "docx")
  
  if(isTRUE(path_is_docx)) {
    sheet_name <- sheet_names <- NULL
  } else {
    sheet_names <- openxlsx::getSheetNames(path)
    sheet_name <- ifelse(is.character(sheet), sheet, sheet_names[sheet])
    if(is.na(sheet_name)) {
      cli::cli_alert_info("Available sheet names: {.val {sheet_names}}")
    } else {
      cli::cli_alert_info("Sheet name: {.val {sheet_name}}")
    }
  }
  
  assertthat::assert_that(
    length(path) == 1,
    is.character(path),
    file.exists(path),
    stringr::str_detect(tolower(fs::path_ext(path)), "xls|docx"),
    msg = "`path` should be a single existing Excel file or docx file."
  )
  
  if(isTRUE(path_is_docx)) {
    requirements <- read_docx_tables(
      path = path,
      docx_header_pattern = docx_header_pattern,
      ...
    )
  } else {
    requirements <- openxlsx::read.xlsx(
      path,
      sheet = sheet,
      ...
    )
  }
  
  # apply naming function
  if(!is.null(make_clean_names_fn)) {
    if(inherits(make_clean_names_fn, "function")) {
      # function: apply the function with the requirements data frame as input.
      names(requirements) <- do.call(make_clean_names_fn, args = list(names(requirements)))
    } else if(inherits(make_clean_names_fn, "formula")) {
      # formula: convert to function then apply with requirements data frame as input.
      names(requirements) <- do.call(
        what = rlang::as_function(make_clean_names_fn),
        args = list(names(requirements))
      )
    } else {
      cli::cli_warn(c(
        "No renaming method defined in {.fun read_requirements} for class {.class {class(make_clean_names_fn)}}.",
        "Names will be returned as-is."
      ))
    }
  }
  
  # variables to drop from the requirements. 
  # (based on internal experience, some historical)
  requirements_drop_vars <- c("values_used_for_the_dynamic_print_area")
  
  requirements <- requirements %>% 
    dplyr::select(-dplyr::any_of(requirements_drop_vars))
  
  # attempt to apply subset criteria
  requirements_subset <- try(
    {
      requirements %>% 
        dplyr::filter(
          {{ subset }} %||% TRUE
        )
    },
    silent = TRUE
  )
  
  if(inherits(requirements_subset, "try-error")) {
    cli::cli_warn(
      c(
        "The provided {.arg subset} resulted in an error. The full contents of the imported file will be returned. The captured error is below:",
        utils::capture.output(attr(requirements_subset, "condition"))
      )
    )
  } else {
    if(!identical(requirements, requirements_subset)) {
      subset_character <- as.character(as.expression(as.list(sys.call())[["subset"]]))
      cli::cli_alert_success("Applied requested subset: {.code {subset_character}}")
    }
    requirements <- requirements_subset
  }
  rm(requirements_subset)
  
  # attempt to apply various attributes
  requirements <- as_requirements(
    .data = requirements,
    variable_name_col = variable_name_col,
    variable_label_col = variable_label_col,
    decode_col = decode_col
  )
  
  attr(requirements, "path") <- path
  
  structure(requirements, class = unique(c("requirements", class(requirements))))
  
}


#' @rdname requirements
#' 
#' @param .data data frame to apply requirements attributes to.
#' 
#' @export
as_requirements <- function(
    .data,
    variable_name_col = "variable_name",
    variable_label_col = "variable_label",
    decode_col = "format_decode"
) {
  
  if(!is.null(variable_name_col)) {
    variable_name <- .data[[variable_name_col]]
    if(is.null(variable_name)) {
      cli::cli_warn(
        "No variable called {.var {variable_name_col}}"
      )
    }
  } else {
    variable_name <- NULL
  }
  
  if(!is.null(variable_label_col)) {
    variable_label <- .data[[variable_label_col]]
    if(is.null(variable_label)) {
      cli::cli_warn(
        "No variable called {.var {variable_label_col}}"
      )
    }
  } else {
    variable_label <- NULL
  }
  
  if(!is.null(decode_col)) {
    decode <- .data[[decode_col]]
    if(is.null(decode)) {
      cli::cli_warn(
        "No variable called {.var {decode_col}}"
      )
    }
  } else {
    decode <- NULL
  }
  
  # warn about long names.
  if(!is.null(variable_name)) {
    long_names <- nchar(variable_name) > 8
    long_names[is.na(long_names)] <- FALSE
    if(any(long_names)) {
      cli::cli_warn(
        c(
          "{length(long_names)} variables with names longer than 8 characters: {.val {variable_name[long_names]}}"
        )
      )
    }
  }
  
  # attempt to apply labels_named_list attribute
  if(!is.null(variable_name) && !is.null(variable_label)) {
    variable_label <- trimws(variable_label)
    long_labels <- nchar(variable_label) > 40
    long_labels[is.na(long_labels)] <- FALSE
    if(any(long_labels)) {
      # warning provides variable names for the long labels.
      cli::cli_warn(
        c(
          "{length(long_labels)} variables with labels longer than 40 characters: {.val {variable_name[long_labels]}}"
        )
      )
    }
    
    # remove blank labels
    remove_labels_lgl <- variable_label == "" | is.na(variable_label)
    labels_named_list <- as.list(variable_label[!remove_labels_lgl])
    names(labels_named_list) <- variable_name[!remove_labels_lgl]
    
    attr(.data, "labels_named_list") <- labels_named_list
    cli::cli_alert_success("Applied the {.val labels_named_list} attribute")
  }
  
  # attempt to apply decode_tbls attribute
  if(!is.null(variable_name) && !is.null(decode)) {
    
    requirements_decode_tbls <- try(
      {
        extract_decode_tbls(
          variable_name = variable_name,
          decode = decode
        )
      },
      silent = TRUE
    )
    
    if(inherits(requirements_decode_tbls, "try-error")) {
      cli::cli_warn(
        c(
          "Extracting decodes resulted in an error. No {.code decode_tbls} attribute will be set. The captured error is below:",
          utils::capture.output(attr(requirements_decode_tbls, "condition"))
        )
      )
    } else {
      attr(.data, "decode_tbls") <- requirements_decode_tbls
      cli::cli_alert_success("Applied the {.val decode_tbls} attribute")
    }
    rm(requirements_decode_tbls)
    
  }
  
  structure(.data, class = unique(c("requirements", class(.data))))
  
}


#' @rdname requirements
#' @export
available_requirements_table <- function(
    path = ".",
    pattern = "req",
    sheet = "specs",
    date_format = c("ymd", "mdy", "dmy"),
    drop_qc = TRUE
  ) {
  
  assertthat::assert_that(
    length(path) == 1,
    is.character(path),
    file.exists(path),
    msg = "`path` should be a single existing file or directory."
  )
  
  assertthat::assert_that(
    length(pattern) == 1,
    is.character(pattern),
    msg = "`pattern` should be a single character pattern."
  )
  
  assertthat::assert_that(
    is.null(sheet) || is.character(sheet) || is.numeric(sheet),
    msg = "`sheet` should be either NULL, a character vector of required sheet names, or an ignored numeric vector."
  )
  
  assertthat::assert_that(
    is.logical(drop_qc),
    length(drop_qc) == 1,
    msg = "`drop_qc` should be a single logical value."
  )
  
  # set visible bindings
  path_date <- created <- modified <- date <- sheets <- is_qc <- NULL
  
  # default empty table to return until the table is built
  requirements_table <- tibble::tibble(
    path = character(),
    path_date = lubridate::Date(),
    created = lubridate::POSIXct(),
    modified = lubridate::POSIXct(),
    date = lubridate::POSIXct(),
    sheets = list(character()),
    is_qc = logical()
  )
  
  # get files that match criteria.
  # when path is a file, use that file.
  # when path is a directory, use pattern to list matching files.
  path_matches <- if(dir.exists(path)) {
    list.files(
      path = path,
      pattern = pattern,
      full.names = TRUE,
      ignore.case = TRUE
    )
  } else {
    # full path already provided
    path
  }
  
  # this function family is only designed to work with certain types of files.
  # currently, only excel and word (docx only) files are supported.
  path_matches <- path_matches[stringr::str_detect(tolower(fs::path_ext(path_matches)), "xls|docx")]
  
  # remove 'tilde' files that start with '~' (indication that the file is or
  # has been open, but not the valid excel file we are looking for).
  path_matches <- path_matches[stringr::str_detect(basename(path_matches), "^~", negate = TRUE)]
  
  # message when no matching files are found.
  no_files_found_message <- "No matching requirements files were found. Supported file types are Excel files and docx files."
  
  if(length(path_matches) == 0) {
    cli::cli_warn(no_files_found_message)
    return(invisible(requirements_table))
  }
  
  # remove double file separators
  path_matches <- stringr::str_replace_all(path_matches, "//", "/")
  
  # define the function that will be used to parse dates from file names
  date_parse_function <- switch(
    match.arg(date_format),
    ymd = lubridate::ymd,
    mdy = lubridate::mdy,
    dmy = lubridate::dmy
  )
  
  # define the date pattern to extract.
  # all patterns include hyphen, underscore, and dot as possible separators.
  # years can be 2-4 digits.
  # months can be 1-2 digits.
  # days can be 1-2 digits.
  date_pattern <- switch(
    match.arg(date_format),
    ymd = "\\d{2,4}[-_\\.]\\d{1,2}[-_\\.]\\d{1,2}",
    mdy = ,
    dmy = "\\d{1,2}[-_\\.]\\d{1,2}[-_\\.]\\d{2,4}",
  )
  
  # extract the expected dates from file paths
  path_date <- purrr::map(
    .x = basename(path_matches),
    .f = function(p) {
      d <- unlist(stringr::str_extract_all(p, date_pattern))
      d <- suppressWarnings(date_parse_function(d))
      d <- d[!is.na(d)]
      if(length(d) == 0) {
        return(lubridate::NA_Date_)
      }
      # we could return the closest date, or the latest date, but we elect
      # to keep only the last date included in the file name.
      d[[length(d)]]
    }
  ) %>% 
    purrr::reduce(c)
  
  # gather information about each matching file
  requirements_table <- tibble::tibble(
    path = path_matches,
    path_date = path_date,
    created = file.info(path_matches)$ctime,
    modified = file.info(path_matches)$mtime,
  ) %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(
      date = dplyr::coalesce(path_date, lubridate::date(modified)),
      sheets = list(
        if(stringr::str_detect(tolower(fs::path_ext(path)), "xls")) openxlsx::getSheetNames(path)
      ),
      is_qc = stringr::str_detect(tolower(basename(path)), "qc|marked")
    ) %>% 
    dplyr::ungroup()
  
  # remove qc files if requested
  if(isTRUE(drop_qc)) {
    requirements_table <- requirements_table %>% 
      dplyr::filter(!is_qc)
  }
  
  # subset to requested sheets.
  # user needs to intentionally pass `sheet = NULL` to include docx files.
  if(!is.null(sheet)) {
    
    if(is.character(sheet)) {
      requirements_table <- requirements_table %>% 
        dplyr::rowwise() %>% 
        dplyr::filter(
          all(sheet %in% sheets)
        ) %>% 
        dplyr::ungroup()
    }
    
  }
  
  if(nrow(requirements_table) == 0) {
    cli::cli_warn(no_files_found_message)
    return(invisible(requirements_table))
  }
  
  requirements_table
  
}


# example requirements ----------------------------------------------------

# intended to be used for testing and in the vignette
example_requirements <- suppressMessages({
  as_requirements(
    tibble::tribble(
      ~variable_name, ~variable_label,             ~pk_ard, ~pk_mif, ~format_decode, 
      "STUDYID",      "Study Identifier",          "x",     NA,      NA,
      "USUBJID",      "Unique Subject Identifier", "x",     NA,      NA,
      "ID",           "Subject ID",                "x",     "x",     NA,
      "AGECAT",       "Baseline Age Category",     "x",     "x",     "1=18-50\n2=51-69\n3=70+",
      "RACEN",        "Race",                      "x",     "x",     "1=White/Caucasian\n2=Black/African American\n3=Asian-Japanese\n4=Asian-Korean\n5=Asian-other\n6=American Indian or Alaska Native\n7=Native Hawaiian or Other Pacific Islander\n8=Other",
      "SEXF",         "Sex",                       "x",     "x",     "0=Male\n1=Female",
      "FED",          "Fed",                       "x",     "x",     "0=Fasted\n1=Fed"
    )
  )
})
