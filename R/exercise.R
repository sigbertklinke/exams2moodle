#' @rdname exercise
#' @title exercise
#' @description Data structure for exercise data
#'
#' @param exer exercise object (default: \code{NULL})
#' @param ... further named parameter to add to the exercise object 
#'
#' @return exercise object
#' @export
#'
#' @examples
#' exer <- exercise()           # new exercise
#' exer <- exercise(exer, x=3)  # add x to the exercise
exercise <- function(exer, ...) { UseMethod("exercise") }

#' @rdname exercise
#' @export
exercise.default <- function(exer=NULL, ...) {
  if (is.null(exer)) exer <- structure(list(), class=c("exercise", "list"))
  stopifnot("exercise" %in% class(exer))
  args  <- list(...)
  if (length(args)) {
    nargs <- names(args)
    if (is.null(nargs)) stop("only named parameters allowed")
    for (arg in nargs) {
      if (!is.null(exer[[arg]])) warning (sprintf("component '%s' overwritten", arg))
      exer[[arg]] <- args[[arg]]
    }
  }
  exer
}