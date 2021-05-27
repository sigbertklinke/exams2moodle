#' @rdname as_string
#' @title  as_string
#' @description converts a character vector in a single string
#' 
#' @param txt vector: (character) vector to merge 
#' @param collapse character: glue text between elements (default: \code{", "})
#' @param last character: glue text between the two last elements (default: \code{", and "})
#' @param name character: observation name (default: \code{"x"})
#' @param sorted logical: sorted observations or not (default: \code{FALSE})
#' @param ... further given from \code{as_obs} to \code{as_string}
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
#' #
#' as_obs(x)
#' as_obs(sort(x), sorted=TRUE)
as_string <- function (txt, collapse=", ", last=", and ") {
  n <- length(txt)-1
  collapse <- c(rep(collapse, length.out=n), '')
  if (is.character(last)) collapse[n] <- last
  paste0(paste0(txt, collapse), collapse="")
}

#' @rdname as_string
#' @export
as_obs <- function(txt, name="x", sorted=FALSE, ...) {
  txt <- paste0('$', name, '_{', if(sorted) '(' else '', 1:length(txt), if(sorted) ')' else '', '}=', txt, '$')
  as_string(txt, ...)
}