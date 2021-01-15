#' @rdname approx
#' @title Approximations
#' @description Functions which deliver `TRUE` or `FALSE` if any approximation if possible. 
#' The approximation parameter `c`can be set directly or via `getOption`. The approximation functions deliver `TRUE`, if
#' 
#' * `t2norm`: `n>c` with `c=30`
#' * `binom2norm`: if `type` is `"single"` (default) then checks `size*prob*(1-prob)>c` else checks `size*prob>c` and `size*(1-prob)>c` with `c=9`
#' * `clt2norm`: `n>c` with `c=30`. Note that existence of the expectation and variance, which is required by the Central Limit Theorem can not be checked. 
#' ``
#' @param n integer: number of observations
#' @param size integer: number of observations
#' @param prob numeric: probability of success on each trial
#' @param type character: approximation condition used
#' @param c numeric: approximation parameter (default: `getOption("distribution.APPROXFUN)` or a default value)
#'
#' @return logical if the approximation would be possible
#' @export
#' @md
#'
#' @examples
#' # check for 5 observations
#' t2norm(n=c(5,50))
#' binom2norm(size=c(5,50), prob=0.5)
#' binom2norm(size=c(5,50), prob=0.5, type="double")
t2norm <- function(n, c=getOption("distribution.t2norm", 30)) {
  (n>c)
}

#' @rdname approx
#' @export
binom2norm <- function(size, prob, c=getOption("distribution.binom2norm", 9), type=c("single", "double")) {
  type <- match.arg(type)
  if (type=="single") return(size*prob*(1-prob)>c)
  return((size*prob>c) & (size*(1-prob)>c))
}

#' @rdname approx
#' @export
clt2norm <- function(n, c=getOption("distribution.clt2norm", 30)) {
  (n>c)
}