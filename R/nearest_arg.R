#' nearest_arg
#'
#' It determines the nearest candidate value for each value in \code{arg}. As a replacement for [base::match.arg]
#' since it is more (typo) error tolerant, but if a wrong choice will be done it is difficult to detect.
#'
#' @param arg character: vector or NULL
#' @param choices character: vector of candidate values
#' @param method character: method for distance calculation (default: \code{cosine})
#' @param ... further parameters for [stringdist::stringdistmatrix]
#'
#' @return for each value in \code{arg} the (first) nearest element of \code{choices} 
#' @importFrom stringdist stringdistmatrix
#' @export
#'
#' @examples
#' # match.arg("tow.sided", c("two.sided", "less", "greater")) # will fail
#' nearest_arg("tow.sided", c("two.sided", "less", "greater")) 
#' nearest_arg(c("two.sided", "less", "greater"), c("two.sided", "less", "greater"))
#' nearest_arg(c("two", "two", "ded", "ss", "ea"), c("two.sided", "less", "greater"))
nearest_arg <- function(arg, choices, method="cosine", ...) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]], envir = sys.frame(sysP))
  }
  if (is.null(arg)) return(choices[1L])
  if (!is.character(arg)) stop("'arg' must be NULL or a character vector")
  if (length(arg) == 0L) stop("'arg' must be of length >= 1")
  d <- stringdistmatrix(arg, choices, method = method, ...)
  choices[apply(d, 1, which.min)]
}