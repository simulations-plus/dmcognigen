#' Set variable labels
#'
#' @param data \code{data.frame} or \code{tibble}
#' @param labels named \code{list} or \code{character} vector. Alternatively, 
#'   the result of \code{read_requirements()} can be used directly.
#'
#' @return object with the same structure as \code{data}, with label attributes
#'   assigned to variables provided in \code{labels}
#' 
#' @export
#'
#' @examples
#' str(mtcars)
#' 
#' custom_labels <- list(
#'   mpg = "Miles per gallon", 
#'   cyl = "Number of cylinders"
#' )
#' 
#' # set labels for an existing object in-place
#' custom_mtcars <- mtcars
#' set_labels(custom_mtcars) <- custom_labels
#' str(custom_mtcars)
#' 
#' # or assign a labelled dataset to an object
#' labelled_mtcars <- set_labels(mtcars, custom_labels)
#' str(labelled_mtcars)
#' 
#' # or with the pipe
#' labelled_mtcars <- mtcars %>% 
#'   set_labels(custom_labels)
#' str(labelled_mtcars)
set_labels <- function(data, labels) {
  
  # check inputs
  assertthat::assert_that(
    is.data.frame(data),
    msg = "data must be a data.frame"
  )
  
  if(inherits(labels, "requirements")) {
    labels <- attr(labels, "labels_named_list")
  }
  
  # transform labels if provided as a character vector
  if(!is.list(labels) && is.character(labels)) {
    labels <- as.list(labels)
  }
  
  assertthat::assert_that(
    is.list(labels),
    !is.data.frame(labels),
    msg = "labels must be a list (non-data.frame)"
  )
  
  value_dim_check <- vapply(
    labels, 
    function(x) is.character(x) & length(x) == 1,
    logical(1L)
  )
  
  assertthat::assert_that(
    length(value_dim_check) > 0,
    all(value_dim_check),
    msg = "Each label must be a character with length 1"
  )
  
  # get variable names
  vars <- names(labels)
  nvars <- length(labels)
  
  assertthat::assert_that(
    all(vars != ""),
    msg = "All labels must be named"
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
  for ( ivar in seq_along(labels) ) {
    attr(data[[vars_in_data[ivar]]], "label") <- labels[[ivar]]
  }
  
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
  
  data
  
}

#' @rdname set_labels
#' 
#' @param value (labels) named \code{list} or \code{character} vector
#' @export
"set_labels<-" <- function(data, value) {
 
 set_labels(data, value)
 
}
