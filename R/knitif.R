#' knitif
#'
#' Selects a text argument and returns the knitted result.
#'
#' @param n character: text argument to use
#' @param ...  character: arguments to choose from
#' @param envir environment: in which code chunks are to be evaluated (default: `[knitr::knit_global]`) 
#'
#' @return a character
#' @importFrom knitr knit_global knit
#' @export
#'
#' @examples
#' knitif(runif(1)<0.5, 'TRUE'="`r pi`", 'FALSE'="$\\pi=`r pi`$")
knitif <- function (n, ..., envir=knit_global()) {
  n    <- as.character(n)
  args <- list(...)
  if (is.null(args[[n]])) stop(sprintf("'%s' not found in arg list", n))
  knit(text=args[[n]], envir=envir)
}