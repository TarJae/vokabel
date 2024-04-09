### general utility functions ###

#' Check if a value is truthy
#'
#' A value is truthy unless it is FALSE, NA, NULL, an empty data.frame, or empty list.
#'
#' Modified from shiny::isTruthy
#'
#' @param x An expression whose truthiness value we want to determine
#'
#' @return boolean
#'
#' @author Joseph Marlo
#' @seealso [shiny::isTruthy()]
#' @noRd
#' @keywords internal
#'
#' @examples
#' vokabel:::is_truthy(TRUE)
#' vokabel:::is_truthy(FALSE)
#' vokabel:::is_truthy(1)
#' vokabel:::is_truthy(0)
#' vokabel:::is_truthy(NULL)
#' vokabel:::is_truthy(NA)
#' vokabel:::is_truthy(data.frame())
#' vokabel:::is_truthy(data.frame(x = 1))
is_truthy <- function(x){
  
  ##### additions
  if (inherits(x, "data.frame") && nrow(x) == 0)
    return(FALSE)
  if (inherits(x, "list") && length(x) == 0)
    return(FALSE)
  
  ##### shiny::isTruthy
  if (inherits(x, "try-error"))
    return(FALSE)
  if (!is.atomic(x))
    return(TRUE)
  if (is.null(x))
    return(FALSE)
  if (length(x) == 0)
    return(FALSE)
  if (all(is.na(x)))
    return(FALSE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x))))
    return(FALSE)
  if (inherits(x, "shinyActionButtonValue") && x == 0)
    return(FALSE)
  if (is.logical(x) && !any(stats::na.omit(x)))
    return(FALSE)
  return(TRUE)
}


#' Check to see if all objects are equal
#'
#' [all.equal()] for arbitrary amount of objects
#'
#' @param objs a list of objects to test 
#'
#' @return boolean
#' @noRd
#' @keywords internal
#'
#' @examples
#' is_set_all_equal(1, 1, 1)
#' is_set_all_equal(1, 1, 2)
#' is_set_all_equal(mtcars, mtcars)
is_set_all_equal <- function(...){
  dot_list <- list(...)
  reference <- dot_list[[1]]
  
  for (obj in dot_list[-1]) {
    if (!isTRUE(all.equal(reference, obj))) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# to satisfy CMD CHECK when using pipe variables
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      'your_answer'
    )
  )
}

