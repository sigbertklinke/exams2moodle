#' Distributions
#'
#' A data frame with the R function names, LaTeX names, discreteness and package origin of a distribution.
#'
#' @docType data
#'
#' @usage data(distributions)
#'
#' @format A data frame with columns \code{r}, \code{latex}, \code{discret} and \code{package}
#'
#' @keywords datasets
#'
#' @examples
#' data(distributions)
#' distributions
"distributions"

#' skalenniveau
#'
#' A data frame with the variables and level of measurement type. The names are in German.
#'
#' @docType data
#'
#' @usage data(skalenniveau)
#'
#' @format A data frame with columns \code{var}, and \code{type}
#'
#' @keywords datasets
#'
#' @examples
#' data(skalenniveau)
#' head(skalenniveau)
"skalenniveau"

#' @rdname sos
#' @title Precomputed sum of squares data 
#' @description
#' Five data matrices with precomputed results from \code{sumofsquares(n, 10, zerosum=TRUE, maxt=Inf)} for
#' \code{n=100}, \code{n=200}, \code{n=400},\code{n=800}, and \code{n=1000}.
#'
#' @docType data
#'
#' @usage data(sos)
#'
#' @format For each line of a matrix holds \eqn{\sum_{i=1}^k x_i^2=n} and \eqn{\sum_{i=1}^k x_i=0}. 
#' It contains all integer solutions up to \code{k<=10}. A \code{NA} means that this entry is not used.
#'
#' @keywords datasets
#'
#' @examples
#' data(sos)
#' head(sos100)
#' rowSums(sos100^2, na.rm=TRUE)
#' rowSums(sos100, na.rm=TRUE)
"sos100"

#' @rdname sos
"sos200"

#' @rdname sos
"sos400"

#' @rdname sos
"sos800"

#' @rdname sos
"sos1000"