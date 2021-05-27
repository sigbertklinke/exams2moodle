#' @title mcval
#' @rdname mcval
#' @description Computes all modes (most common value).
#'
#' @param x data object
#' @param exact logical: compute exact mode or use class mids (default: \code{FALSE})
#' @param ... unused
#'
#' @md
#' @return a vector of modes
#' @export
#'
#' @examples
#' x <- sample(1:5, 15, replace=TRUE)
#' mcval(x)
mcval <- function(x, ...) {
  UseMethod("mcval")
}

#' @rdname mcval
#' @importFrom methods as
#' @export
mcval.default <- function(x, ...) {
  tab <- table(x)
  ret <- names(tab)[which(tab==max(tab))]
  as(ret, class(x))
}

#' @rdname mcval
#' @export
mcval.histogram <- function(x, exact=FALSE, ...) {
  modes <- which(x$density==max(x$density))
  ret   <- x$mids[modes]
  if (exact) {
    fl  <- ifelse(modes==1, 0, x$density[modes-1])
    fc  <- x$density[modes]
    fr  <- ifelse(modes==length(x$density), 0, x$density[modes+1])
    ret <- x$breaks[modes]+(fc-fr)/(2*fc-fr-fl)*(x$breaks[modes+1]-x$breaks[modes])
  } 
  attr(ret, "pos") <- modes
  ret
}