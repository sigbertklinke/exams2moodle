#' @title Results with rounding
#' @description Rounds \code{x} according to \code{digits}, \code{FUN} and sets a tolerance for the result. 
#' If the tolerance not given it is the maximum of \code{2*10^(-digits)}.
#' @param x numeric: value to round
#' @param digits integer or character: Digits that should be used for rounding or \code{"integer"} for \code{digits=0}, \code{"\%"} for \code{digits=2},  or  \code{"probability"} for \code{digits=4}. Abbreviations for the names can be used
#' @param tol numeric: tolerance for result
#' @param FUN function: rounding function (default: \code{round2})
#'
#' @return a list with original and rounded value, digits used and tolerance
#' @importFrom exams round2
#' @export
#'
#' @examples
#' x <- as_result(1/3, "prob")
#' tol(x)
#' rounded(x)
#' tol(x)
#' digits(x)
as_result <- function(x, digits, tol=NA, FUN=round2) {
  stopifnot(is.numeric(x))
  if (is.character(digits)) {
    pos <- pmatch(digits, c("integer", "probability", "%"))
    if (pos==1) digits <- 0
    if (pos==2) digits <- 4
    if (pos==3) digits <- 2
    stopifnot(is.numeric(digits))
  }
  tol[is.na(tol)] <- 2*10^(-digits)
  ret <- list(x=x, r=FUN(x, digits), digits=digits, tol=max(tol))
  structure(ret, class=c("result", class(ret)))
}

#' @rdname as_result
#' @export
tol <- function(x) {
  stopifnot("result" %in% class(x))
  x$tol
}

#' @rdname as_result
#' @export
rounded <- function(x) {
  stopifnot("result" %in% class(x))
  x$r
}

#' @rdname as_result
#' @export
val <- function(x) {
  stopifnot("result" %in% class(x))
  x$x
}

#' @rdname as_result
#' @export
digits <- function(x) {
  stopifnot("result" %in% class(x))
  x$digits
}