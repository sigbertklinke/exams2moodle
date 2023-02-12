histbreaks(breaks, size, outer = TRUE)

histwidth(from, to, widths, dmax = 100)

histx(breaks, n, alpha = 0.99)

sample_size_freq(n, f, which = NA)


histdata(x, breaks = "Sturges", probs = seq(0, 1, 0.25), ...)

## S3 method for class 'histogram'
quantile(x, probs = seq(0, 1, 0.25), ...)

## S3 method for class 'histogram'
median(x, ...)

## S3 method for class 'histogram'
mean(x, ...)

hist(x, breaks = "Sturges",
     freq = NULL, probability = !freq,
     include.lowest = TRUE, right = TRUE, fuzz = 1e-7,
     density = NULL, 
     main = paste("Histogram of" , xname),
     xlim = range(breaks), ylim = NULL,
     xlab = xname, ylab,
     axes = TRUE, plot = TRUE, labels = FALSE,
     nclass = NULL, warn.unused = TRUE, ...)

hist_breaks <- function(x, 
                        size=0,  outer=TRUE,      # breaks
                        widths=NULL, tol=1e-6,    # widths
                        ) {
  if (size>2) {
    stopifnot(size<=length(x))
    if (outer) {
      breaks <- x
      if (size>3) {
        breaks <- range(x)
        breaks <- c(breaks, sample(setdiff(x, breaks), size-2))
      } 
    } else {
      breaks <- sample(x, size-2)
    }
  } else {
    stopifnot(length(widths)>0)
    rg <- diff(range(x))
    nb <- ceiling(rg/min(widths))
    repeat {
      breaks <- rg[1] + c(0, cumsum(sample(widths, nb, replace = TRUE)))
      breaks <- breaks[breaks <= to]
      if (max(breaks) < rg[2]) breaks <- c(breaks, rg[2])
      ws <- diff(breaks)
      if (any(equal(ws[length(ws)], widths, tol))) break
    }
  }
  sort(breaks)
}

hist_data(x, breaks="Scott", probs=seq(0, 1, 0.25), # quantile 
          n=NULL, condtype=0, unit=10, ...) {
  stopifnot(is.numeric(n))
  type <- match.arg(tolower(type), c("scott", "sturges", "fd", "quantile"))
  if (length(n)>0) {
    if (length(n)>1) {
      f <- ddiscrete(runif(length(n)), unit=unit)
      
    }
    
  } else {
    
  }
    stopifnot(is.numeric(breaks))
    nb <- length(breaks)
    stopifnot(nb == 1 + length(n))
    xm <- (breaks[-1] + breaks[-nb])/2
    xd <- diff(breaks)
    x <- numeric(0)
    for (i in 1:length(n)) {
      x <- c(x, runif(n[i], min = xm[i] - alpha * xd[i]/2, 
                      max = xm[i] + alpha * xd[i]/2))
    }
    sample(x, length(x))
  } else {
    
  }
    

  function (x, breaks = "Sturges", probs = seq(0, 1, 0.25), ...) 
  {

    xname <- paste(deparse(substitute(x), 500), collapse = "\n")
    x <- x[is.finite(x)]
    args <- list(...)
    if (is.null(args$right)) 
      args$right <- TRUE
    args$x <- x
    args$breaks <- breaks
    args$plot <- FALSE
    ret <- do.call("hist", args)
    stopifnot((min(ret$breaks) <= min(x)) && (max(ret$breaks) >= 
                                                max(x)))
    ret$width <- as.numeric(diff(ret$breaks))
    ret$x <- x
    ret$xname <- xname
    ret$mids <- as.numeric(ret$mids)
    ret$relfreq <- ret$counts/sum(ret$counts)
    ret$cumfbrk <- c(0, ret$relfreq)
    ret$class <- findInterval(ret$x, ret$breaks, left.open = args$right, 
                              all.inside = TRUE)
    ret$lower <- as.numeric(ret$breaks[-length(ret$breaks)])
    ret$upper <- as.numeric(ret$breaks[-1])
    ret
  }
      
  }
  
  