#' @title histdata
#' @rdname histdata
#' @description Returns data for a histogram, calls internally `hist(..., plot=FALSE)`. 
#' `mean` returns the mean of the data
#' `quantile` and `median` return the quantile(s) or median with an attribute `pos`, the class number of the qauntile(s) or the median. 
#' 
#' @inheritParams graphics::hist
#' @param x numeric data or histogram data
#' @param probs numeric: probabilities to use if `breaks="Quantile"` (default: `seq(0, 1, 0.25)`)
#' @param ... further parameter used in [graphics::hist] 
#'
#' @return like in [graphics::hist] with the additional list elements
#' * `lower` lower class borders,
#' * `upper` upper class borders, 
#' * `width` class widths,
#' * `relfreq` the relative class frequency,
#' * `cumfbrk` the cumulated relative frequency at the `breaks`,
#' * `maxdens` the indices of the maximal `density` values,
#' * `maxcount` the indices of the maximal `count` values
#' * `x` the original finite data, and
#' * `class` the class number for each value in `x`.
#' 
#' @md
#' @export
#'
#' @examples
#' #1
#' x <- seq(0, 1, by=0.25)
#' print(hist(x, plot=FALSE))
#' histdata(x)
#' #2
#' x <- seq(0, 1, by=0.25)
#' print(hist(x, x, plot=FALSE))
#' histdata(x, x)
#' #3
#' print(hist(x, x, right=FALSE, plot=FALSE))
#' histdata(x, x, right=FALSE)
histdata <- function(x, breaks="Sturges", probs=seq(0, 1, 0.25), ...) {
  stopifnot(is.numeric(x))
  xname <- paste(deparse(substitute(x), 500), collapse = "\n")
  x <- x[is.finite(x)]
  args <- list(...)
  if (is.null(args$right)) args$right <- TRUE
  args$x <- x
  args$breaks <- breaks
  args$plot   <- FALSE
  ret <- do.call("hist", args)
  stopifnot((min(ret$breaks)<=min(x)) && (max(ret$breaks)>=max(x))) # check if breaks cover the data
  ret$width    <- as.numeric(diff(ret$breaks))
  ret$x        <- x
  ret$xname    <- xname
  ret$mids     <- as.numeric(ret$mids)
  ret$relfreq  <- ret$counts/sum(ret$counts)
  ret$cumfbrk  <- c(0, ret$relfreq)
  ret$class    <- findInterval(ret$x, ret$breaks, left.open=args$right, all.inside = TRUE)
  ret$lower    <- as.numeric(ret$breaks[-length(ret$breaks)])
  ret$upper    <- as.numeric(ret$breaks[-1])
  ret$maxdens  <- which(ret$density==max(ret$density))
  ret$maxcount <- which(ret$count==max(ret$count))
  ret
}

#' @rdname histdata
#' @export
quantile.histogram <- function(x, probs = seq(0, 1, 0.25), ...) {
  fx  <- x$counts/sum(x$counts)
  Fx  <- c(0, cumsum(fx))
  pos <- colSums(outer(Fx, probs, "<="))
  pos[pos>length(x$counts)] <- length(x$counts)
  ret <- x$breaks[pos]+(probs-Fx[pos])/fx[pos]*(x$breaks[pos+1]-x$breaks[pos])
  attr(ret, "pos") <- pos
  ret
}

#' @rdname histdata
#' @export
median.histogram <- function(x, ...) {
  quantile(x, 0.5)
}

#' @rdname histdata
#' @export
mean.histogram <- function(x, ...) { 
  sum(x$counts*x$mids)/sum(x$counts) 
}
