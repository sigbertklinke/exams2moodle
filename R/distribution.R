#' @rdname Distribution
#' @title Class distribution
#' @description Holds a univariate distribution including its parameters. The name of the distribution is used to determine, for example 
#' the function for quantiles: \code{paste0("q", name)}. Usually the full name has to be used; some abbreviated names are possible
#' \itemize{
#' \item{\code{binom}} binomial distribution, parameters: \code{size}, \code{prob}
#' \item{\code{hyper}} hypergeometric distribution, parameters: \code{m}, \code{n}, \code{k}
#' \item{\code{geom}} geometric distribution, parameters: \code{prob}
#' \item{\code{pois}} Poisson distribution, parameters: \code{lambda}
#' \item{\code{unif}} continuous uniform  distribution, parameters: \code{min}, \code{max}
#' \item{\code{dunif}} discrete uniform  distribution, parameters: \code{min}, \code{max}
#' \item{\code{exp}} exponential distribution, parameter: \code{rate}
#' \item{\code{norm}} normal distribution, parameters: \code{mean}, \code{sd}
#' \item{\code{lnorm}} log-normal distribution, parameters: \code{meanlog}, \code{sdlog}
#' \item{\code{t}} Student t distribution, parameter: \code{df}
#' \item{\code{chisq}} chi-squared distribution, parameter: \code{df}
#' \item{\code{f}} F distribution, parameters: \code{df1},  \code{df2}
#' }
#' Note that a  probability mass/density, quantile and cumulative distribution function must exist.
#' 
#' The following functions exists for \code{disributions}:
#' \itemize{
#' \item{\code{distribution}} creates a distribution with name `name` and parameters
#' \item{\code{quantile}} computes the quantiles of a distribution using `paste0('q', name)`
#' \item{\code{cdf}} computes the cumulative distribution function of a distribution using `paste0('p', name)`
#' \item{\code{pmdf}} computes the probability mass/density function of a distribution using `paste0('d', name)`
#' \item{\code{prob}} computes the probability for a interval between `min` and `max` (`max` included, `min` excluded)
#' \item{\code{prob1}} computes the point probability f
#' \item{\code{is.distribution}} checks if `object` is distribution object. If `name` is given then it checks if distribution type is the same  
#' \item{\code{toLatex}} generates a LaTeX representation of the distribution an its parameter
#' }
#' @param name character: name of the distribution type
#' @param ... further named distribution parameters
#' @param discrete logical: Is distribution discrete? (default: \code{NA}) 
#'
#' @return a distribution object
#' @export
#'
#' @examples
#' d <- distribution("norm", mean=0, sd=1)
#' quantile(d)
#' quantile(d, c(0.025, 0.975))
#' cdf(d, 0)
#' is.distribution(d)
#' is.distribution(d, "t")
#' toLatex(d)
#' # see the LaTeX names
#' data(distributions)
#' distributions
distribution <- function(name, ...) UseMethod("distribution")

#' @rdname Distribution
#' @export
distribution.default <- function(name, ..., discrete=NA) { 
  ret <- list(...)
  ind   <- pmatch(name, distributions$r)
  if (is.na(ind)) {
    ret$name <- name
    discrete <- if (is.na(discrete)) FALSE else discrete
  } else {
    ret$name <- distributions$r[ind]  
    discrete <- if (is.na(discrete)) distributions$discrete[ind] else discrete
  }
  match.fun(paste0("q", ret$name))
  match.fun(paste0("p", ret$name))
  match.fun(paste0("d", ret$name))
  structure(ret, class=c("distribution", class(ret)), discrete=discrete)
}

#' @rdname Distribution
#' @param x distribution
#' @param probs numeric: vector of probabilities with values in [0,1]. 
#' @export
quantile.distribution <- function(x, probs=seq(0, 1, 0.25), ...) {
  fun       <- match.fun(paste0("q", x$name))
  args      <- x
  args$name <- NULL
  args$p    <- probs
  do.call(fun, args)
}

#' @rdname Distribution
#' @param x distribution
#' @param q numeric: vector of quantiles
#' @export
cdf <- function(x, q, ...) {
  stopifnot("distribution" %in% class(x))
  fun       <- match.fun(paste0("p", x$name))
  args      <- x
  args$name <- NULL
  args$q    <- q
  do.call(fun, args)
}

#' @rdname Distribution
#' @param d distribution
#' @param x vector of values
#' @export
pmdf <- function(d, x, ...) {
  stopifnot("distribution" %in% class(d))
  fun       <- match.fun(paste0("d", d$name))
  args      <- d
  args$name <- NULL
  args$x    <- x
  do.call(fun, args)
}

#' @rdname Distribution
#' @param object distribution object
#' @param name character: a replacement of the name of the distribtuion type
#' @param param character: names for the distribution parameters
#' @param digits integer: number of digits used in \code{signif}
#' @importFrom utils toLatex
#' @export
toLatex.distribution <- function(object, name=NULL, param=NULL, digits=3, ...) {
  stopifnot("distribution" %in% class(object))
  if (is.null(param)) param <- rep('', length(object)-1)
  if (is.null(name)) {
    ind <- which(distributions$r==object$name)
    if (length(ind)) {
      type <- distributions$latex[ind]
      if ((type=="N") && (param[2]=='')) param[2] = "\\sigma"
    } else {
      type               <- object$name 
      substr(type, 1, 1) <- toupper(substr(type, 1, 1))
    }
  } else type <- name
  value  <- unlist(object[1:(length(object)-1)])
  params <- paste0(param, ifelse(param=='',  '', '='), signif(value, digits), collapse=", ")
  ret    <- if (endsWith(type, "_"))  paste0(type, "{", params, "}") else paste0(type, "(", params, ")")
  ret
}

#' @rdname Distribution
#' @export
is.distribution <- function(object, name=NULL) {
  ret <- "distribution" %in% class(object)
  if (!ret || is.null(name)) return(ret)
  ind   <- pmatch(name, distributions$r)
  return(object$name==distributions$r[ind])
} 

#' @rdname Distribution
#' @param d distribution
#' @param min numeric: left border of interval
#' @param max numeric: right border of interval
#' @param tol numeric: tolerance for `max==min` (default: \code{1e-6})
#' @export
prob <- function(d, min=-Inf, max=+Inf, tol = 1e-6) {
  stopifnot("distribution" %in% class(d))
  rng <- range(min, max)
  ret <- NA
  if (equal(rng[1], rng[2], tol)) {
    ret <- 0
    if (attr(d, "discrete")) ret <- pmdf(d, rng[1])
  } else {
    ret <- diff(cdf(d, rng))    
  }
  ret
}

#' @rdname Distribution
#' @export
prob1 <- function(d, x, tol=1e-6) { prob(d=d, min=x, max=x, tol=tol) }