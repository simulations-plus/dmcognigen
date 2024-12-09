#' Set variable labels
#'
#' @param data \code{data.frame} or \code{tibble}
#' @param labels,value either a named \code{list} or \code{character} vector, a
#'   \code{requirements} object, or a data frame with labelled variables.
#'   Additionally, \code{NULL} can be used to remove variable labels.
#' @param ... named pairs where the name corresponds to one variable in
#'   \code{data} and the value is either the variable label or a source to 
#'   inherit the label from. This label is kept over an element in \code{labels}.
#'
#' @return object with the same structure as \code{data} with label attributes
#'   assigned to the variables provided in \code{labels} and \code{...}.
#' 
#' @export
#'
#' @examples
#' 
#' # remove all variable labels for examples
#' pk_no_labels <- dmcognigen_pk |>
#'   set_labels(NULL)
#' 
#' # with a requirements object
#' pk_with_labels <- pk_no_labels |>
#'   set_labels(dmcognigen_pk_requirements)
#' 
#' # with another data set
#' pk_with_labels <- pk_no_labels |>
#'   set_labels(dmcognigen_pk)
#'   
#' # by providing individual variable labels with `...`
#' pk_with_labels <- pk_no_labels |>
#'   set_labels(ID = "ID")
#' 
#' # or any combination
#' pk_with_labels <- pk_no_labels |>
#'   set_labels(dmcognigen_pk_requirements, USUBJID = dmcognigen_cov, ID = "ID")
#' 
#' # apply labels from multiple datasets by using the function multiple times
#' pk_with_labels <- pk_no_labels |>
#'   set_labels(dmcognigen_cov) |>
#'   set_labels(dmcognigen_dose) |>
#'   set_labels(dmcognigen_conc)
set_labels <- function(data, labels = list(), ...) {
  
  # check inputs
  assertthat::assert_that(
    is.data.frame(data),
    msg = "`data` must be a data.frame"
  )
  
  # `...` can be used to pass individual variable labels
  dot_list <- rlang::dots_list(
    ...,
    .homonyms = "error"
  )
  
  # when this function iterates over `...`, it is in "dot mode".
  dot_mode <- isTRUE(dot_list[[".dot_mode"]])
  
  # names for warning and error messages
  if(dot_mode) {
    var_name <- dot_list[[".var_name"]]
    label_name <- dot_list[[".label_name"]]
  } else {
    var_name <- NULL
    label_name <- rlang::caller_arg(labels)
  }
  
  dot_list[[".dot_mode"]] <- NULL
  dot_list[[".var_name"]] <- NULL
  dot_list[[".label_name"]] <- NULL
  
  # special cases for existing attributes
  
  # directly from requirements attribute
  if(inherits(labels, "requirements")) {
    cli::cli_alert_info(
      "Inheriting labels from {.arg {label_name}} {.cls requirements} object."
    )
    labels <- attr(labels, "labels_named_list")
  }
  
  # for data frames, first check for variable labels
  if(inherits(labels, "data.frame")) {
    maybe_labels <- purrr::map(labels, attr, which = "label")
    if(isTRUE(!all(purrr::map_lgl(maybe_labels, is.null)))) {
      cli::cli_alert_info("Inheriting labels from variables in {.arg {label_name}} {.cls data.frame}.")
      labels <- maybe_labels
    }
  }
  
  # for other data frames, can try to deframe
  if(inherits(labels, "data.frame")) {
    if(ncol(labels) == 2) {
      cli::cli_alert_info(
        "Inheriting labels from {.val {names(labels)}} variables in {.arg {label_name}} {.cls data.frame}."
      )
      labels <- tibble::deframe(labels)
    } else {
      cli::cli_abort(c(
        "No variable labels detected from the {.arg {label_name}} {.cls data.frame}. ",
        "For this type of input, either provide a {.cls data.frame} that has variables with labels, or two columns representing variable names and variable labels."
      ))
    }
  }
  
  # special case to remove all variable labels
  if(is.vector(labels)) {
    if(all(is.na(labels)) && length(labels) == 1) {
      labels <- NULL
    }
  }
  
  if(is.null(labels) || identical(labels, "")) {
    for(var in names(data)) {
      attr(data[[var]], "label") <- NULL
    }
    var <- NULL
    labels <- list()
  }
  
  # transform labels if provided as a character vector
  if(!is.list(labels) && is.character(labels)) {
    labels <- as.list(labels)
  }
  
  if(!is.list(labels) || is.data.frame(labels)) {
    cli::cli_abort("{.arg labels} of class {.cls {class(labels)}} could not be interpreted.")
  }
  
  # remove NULL elements (unless in dot mode, where labels can be dropped).
  if(dot_mode) {
    labels <- purrr::discard(labels, is.null)
  }
  
  if(length(labels) > 0) {
    
    # NULL and NA values are permitted here, but are only kept in dot mode,
    # when provided explicitly.
    labels_with_issues <- purrr::discard(labels, function(x) {
      suppressWarnings({
        is.null(x) || is.na(x) || (is.character(x) & length(x) == 1)
      })
    })
    
    if(length(labels_with_issues) > 0) {
      reasons_not_allowed <- purrr::map_chr(names(labels_with_issues), function(nm) {
        x <- labels_with_issues[[nm]]
        cli::format_inline("{nm}: {.cls {class(x)}} with length {length(x)}")
      })
      cli::cli_abort(c(
        "Labels could not be interpreted because they are not {.cls character} with length 1: ",
        reasons_not_allowed
      ))
    }
    
    # get variable names
    vars <- names(labels)
    nvars <- length(labels)
    
    assertthat::assert_that(
      all(vars != ""),
      !is.null(vars),
      msg = "All labels should be named."
    )
    
    # check for duplicated vars with differing labels
    vars_duplicated_lgl <- duplicated(vars)
    if( any(vars_duplicated_lgl) ) {
      
      vars_duplicated_names <- unique(vars[vars_duplicated_lgl])
      vars_identical_value_lgl <- vapply(
        vars_duplicated_names, 
        function(v) length(unique(labels[vars == v])) == 1L,
        logical(1L)
      )
      
      assertthat::assert_that(
        all(vars_identical_value_lgl),
        msg = sprintf(
          "Multiple labels provided for the same variable(s):\n %s",
          paste(vars_duplicated_names[!vars_identical_value_lgl], 
                collapse = "\n  ")
        )
      )
      
    }
    
    # filter to names present in data
    vars_in_data <- match(vars, names(data))
    vars_not_in_data <- vars[ is.na(vars_in_data) ]
    
    if ( any(is.na(vars_in_data)) ) {
      labels <- labels[ !is.na(vars_in_data) ]
      vars_in_data <- vars_in_data [ !is.na(vars_in_data) ]
    }
    
    # update attributes
    # NA is updated to "" here, NA values are only expected when provided explicitly.
    labels[is.na(labels)] <- ""
    for ( ivar in seq_along(labels) ) {
      attr(data[[vars_in_data[ivar]]], "label") <- labels[[ivar]]
    }
  }
  
  # apply additional labels that were provided in `...`.
  # these are kept with preference over any previously set labels.
  if(length(dot_list) > 0) {
    
    dot_list_names <- names(dot_list)
    
    if(any(dot_list_names == "")) {
      cli::cli_abort(c(
        x = "All elements of {.arg ...} should be named corresponding to a variable name in {.arg data}."
      ))
    }
    
    dot_list_names <- intersect(dot_list_names, names(data))
    dot_list <- dot_list[dot_list_names]
    
    for(iname in dot_list_names) {
      label <- dot_list[[iname]]
      
      # try collecting the name of the object
      label_name <- try({
        as.character(substitute(list(...))[iname])
      }, silent = TRUE)
      if(inherits(label_name, "try-error")) {
        label_name <- "label"
      }
      
      label <- if(is.data.frame(label) || is.null(label)) {
        label
      } else if(is.vector(label)) {
        purrr::set_names(as.character(label), iname)
      } else {
        # for now, will try using whatever was passed and will keep it named as
        # passed. general error handling will take care of issues. 
        dot_list[iname]
      }
      
      data[iname] <- set_labels(
        data[iname], 
        labels = label,
        # these are secret arguments that allows more control over
        # explicitly named variable names as arguments, used only in this
        # function.
        .dot_mode = TRUE,
        .var_name = iname,
        .label_name = label_name
      )
      
    }
    
  }
  
  # don't warn in dot mode.
  # instead, check and report after applying all labels.
  if(!dot_mode) {
    # warn if any labels in the resulting dataset are greater than 40 characters
    label_lengths <- vapply(
      names(data),
      function(nm) nchar(attr(data[[nm]], "label") %||% ""),
      integer(1L)
    )
    
    if (any(label_lengths > 40)) {
      vars_labels_over_40 <- names(data)[which(label_lengths > 40)]
      cli::cli_warn(c(
        x = "The following variables have labels with character length greater than 40: {vars_labels_over_40}"
      ))
    }
  }
  
  data
  
}

#' @rdname set_labels
#' 
#' @export
"set_labels<-" <- function(data, ..., value) {
 
 set_labels(data, labels = value, ...)
 
}
