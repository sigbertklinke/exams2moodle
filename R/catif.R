#' catif
#'
#' Calls \code{cat} if \code{cond==TRUE}.
#'
#' @param cond logical: condition, if true then \code{cat} is called otherwise not 
#' @param ... further parameter given to \code{cat}
#'
#' @return nothing 
#' @export
#'
#' @examples
#' catif(TRUE, "PDF")      # should appear
#' catif(FALSE, "Moodle")  # should not appear
catif <- function(cond, ...) {
  if (as.logical(cond)) cat(...)
}