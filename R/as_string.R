#' as_string
#' converts a character vector in a single string
#' 
#' @param txt vector: (character) vector to merge 
#' @param collapse character: glue text between elements (default: \code{", "})
#' @param last character: glue text between the two last elements  (default: \code{", and "})
#'
#' @return a string 
#' @export
#'
#' @examples
#' x <- runif(5)
#' y <- c(TRUE, FALSE, NA) 
#' as_string(x)
#' as_string(y)
#' # toString
#' as_string(as.character(x))
#' as_string(as.character(y))
as_string <- function (txt, collapse=", ", last=", and ") {
  n <- length(txt)-1
  collapse <- c(rep(collapse, length.out=n), '')
  if (is.character(last)) collapse[n] <- last
  paste0(paste0(txt, collapse), collapse="")
}
