#' @rdname Distribution
#' @title Class distribution
#' @description Holds one distribution including its parameters. The name of the distribution is used to determine, for example 
#' the function for quantiles: \code{paste0("q", name)}. Usually the full name has to be used; some abbreviated names are possible
#' \itemize{
#' \item{\code{binom}} hypergeometric distribution, parameters: \code{size}, \code{prob}
#' \item{\code{hyper}} hypergeometric distribution, parameters: \code{m}, \code{n}, \code{k}
#' \item{\code{geom}} geometric distribution, parameters: \code{prob}
#' \item{\code{pois}} Poisson distribution, parameters: \code{lambda}
#' \item{\code{unif}} hypergeometric distribution, parameters: \code{min}, \code{max}
#' \item{\code{exp}} exponential distribution, parameter: \code{rate}
#' \item{\code{norm}} normal distribution, parameters: \code{mean}, \code{sd}
#' \item{\code{lnorm}} log-normal distribution, parameters: \code{meanlog}, \code{sdlog}
#' \item{\code{t}} Student t distribution, parameter: \code{df}
#' \item{\code{chisq}} chi-squared distribution, parameter: \code{df}
#' \item{\code{f}} F distribution, parameters: \code{df1},  \code{df2}
#' }
#' Note that a quantile and cumulative distribution function must exist.
#' 
#' The following functions exists for \code{disributions}:
#' \itemize{
#' \item{\code{distribution}} creates a distribution with name `name` and parameters
#' \item{\code{quantile}} computes the quantiles of a distribution using `paste0('q', name)`
#' \item{\code{cdf}} computes the cumulative distribution function of a distribution using `paste0('p', name)`
#' \item{\code{is.distribution}} checks if `object` is distribution object. If `name` is given then it checks if distribution type is the same  
#' \item{\code{toLatex}} generates a LaTeX representation of the distribution an its parameter
#' }
#' @param name character: name of the distribution type
#' @param ... further named distribution parameters
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
distribution <- function(name, ...) UseMethod("distribution")

#' @rdname Distribution
#' @description \code{standarddistributions} returns the names of some standard distributions and their LaTeX representation
#' @param names logical: should only the names of th standard distrutions returned or the LaTeX representations (default: \code{TRUE})
#' @export
standarddistributions <- function(names = TRUE) {
  d <- c("binom"="B", "chisq"="\\chi^2", "exp"="Exp", "f"="F_", "geom"="G", 
         "hyper"="Hyp", "lnorm"="lN", "norm"="N", "pois"="Po", "t"="t_", "unif"="U")
  if (names) return(names(d))
  d 
}

#' @rdname Distribution
#' @export
distribution.default <- function(name, ...) { 
  ret <- list(...)
  distr <- getOption("distribution", standarddistributions())
  ind   <- pmatch(name, distr)
  if (!is.na(ind)) name <- distr[ind]
  match.fun(paste0("q", name))
  match.fun(paste0("p", name))
  ret$name <- name;
  structure(ret, class=c("distribution", class(ret)))
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
#' @param object distribution object
#' @param name character: a replacement of the name of the distribtuion type
#' @param param character: names for the distrubtion parameters
#' @param digits integer: number of digits used in \code{signif}
#' @importFrom utils toLatex
#' @export
toLatex.distribution <- function(object, name=NULL, param=NULL, digits=3, ...) {
  if (is.null(param)) param <- rep('', length(object)-1)
  if (is.null(name)) {
    d   <- standarddistributions(FALSE)
    ind <- which(names(d)==object$name)
    if (length(ind)) {
      type <- d[ind]
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
  distr <- getOption("distribution", standarddistributions())
  ind   <- pmatch(name, distr)
  return(object$name==distr[ind])
} 