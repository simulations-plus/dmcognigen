#' Find objects containing names
#'
#' Find \code{list} objects whose names contain any elements of \code{x}. Note
#' that \code{data.frame} objects are considered \code{list} objects.
#'
#' @param x a \code{character} vector where matches to any elements are sought
#' @param envir which environment to use in listing the available objects.
#'   Defaults to the global environment. This can also be a \code{list}
#'   containing objects to check.
#' @param ignore_case \code{logical} representing whether to ignore case
#'
#' @return returns a named \code{list} of \code{character} vectors containing
#'   the names of \code{list} objects with some names matching to \code{x} or
#'   invisibly returns \code{NULL} where there are no \code{list} objects in
#'   \code{envir}.
#'
#' @export
#'
#' @examples
#' data(mtcars); in_which("cyl")
#' 
#' # check for common subject identifiers, explicitly defining case
#' in_which(
#'   c("ID", "SUBJECT", "SUBJID", "USUBJID"), 
#'   ignore_case = FALSE
#' )
in_which <- function(x, envir = .GlobalEnv, ignore_case = TRUE) {
  objs <- objects(envir)
  
  if (length(objs) == 0) {
    message("No matches (no objects in environment)")
    return(invisible(NULL))
  }
  
  list_objs <- names(which(sapply(objs, function(obj) {
    is.list(eval(parse(text = obj), envir = envir))
  })))
  
  if(length(list_objs) == 0) {
    message("No matches (no list objects in environment)")
    return(invisible(NULL))
  }
  
  # iterate over x
  x_match_this <- if(ignore_case) toupper(x) else x
  match_list <- lapply(x_match_this, function(match_this) {
    # check the names of each object for match_this
    # result is logical vector of length list_objs
    names(which(sapply(list_objs, function(obj) {
      obj_names <- names(eval(parse(text = obj), envir = envir))
      obj_names <- if(ignore_case) toupper(obj_names) else obj_names
      match_this %in% obj_names
    })))
  })
  
  names(match_list) <- x
  
  match_list
  
}
