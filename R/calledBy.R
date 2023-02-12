#' calledBy
#'
#' Checks if the result from [base::sys.calls] contains a call from `fun`.
#'
#' @param fun character: name of the calling function (default: `exams2pdf`)
#'
#' @return logical
#' @export
#'
#' @examples
#' funb <- function() { calledBy('funa') }
#' funa <- function() { funb() }
#' funa()
calledBy <- function(fun='exams2pdf') {
  any(sapply(sys.calls(), function(e) { as.character(e)[1] })==fun)
}