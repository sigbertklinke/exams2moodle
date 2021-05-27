#' hyperloop
#' 
#' Runs a function several times with all parameter combinations:
#' 
#' * If an argument is not a list then it will be converted to a one element list.
#' * If an error occurs then the result of \code{FUN} will not be stored-
#'
#' @param FUN function with named parameter(s)
#' @param ... named parameters which contain lists with possible parameter values
#'
#' @return a hyperloop object
#' @md
#' @export
#'
#' @examples
#' x   <- rnorm(100)
#' trm <- hyperloop(mean, x=list(x), trim=as.list(seq(0, 0.5, by=0.05)))
#' # automatic conversion of x to list(x)
#' trm <- hyperloop(mean, x=x, trim=as.list(seq(0, 0.5, by=0.05))) 
#' unlist(trm)
hyperloop <- function(FUN, ...) {
  args       <- list(...)
  stopifnot(!is.null(names(args)))
  stopifnot(all(names(args)!=""))
  for (i in 1:length(args)) {
    if (!is.list(args[[i]])) args[[i]] <- list(args[[i]])
  }
  hyperindex <- lapply(args, function(e) { 1:length(e) } )
  loop       <- expand.grid(hyperindex)
  ret        <- vector("list", nrow(loop))
  rloop      <- vector("list", nrow(loop))
  keep       <- rep(TRUE, nrow(loop))
  for (i in 1:nrow(loop)) {
    argsi <- list()
    for (j in 1:length(args)) {
      argsi[[j]] <- args[[j]][[loop[i,j]]]
    }
    names(argsi) <- names(args)
    tryCatch({ret[[i]]   <- do.call(FUN, argsi) }, 
             error = function(e) { ret[[i]] <- NULL; keep[i] <- FALSE})
    rloop[[i]] <- loop[i,]
  }
  ret   <- ret[keep]
  rloop <- rloop[keep]
  structure(ret, loop=rloop, class="hyperloop")
}