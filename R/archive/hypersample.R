assign_funenv <- function(l) {
  sf <- sys.frames()
  for (i in (length(sf):1)) {
    if (exists(".PVezKxhB3cKDVLbgWTQ996UDD0HRF", sf[[i]], inherits=FALSE)) {
      sf[[i]]$.PVezKxhB3cKDVLbgWTQ996UDD0HRF <- l
      return(TRUE)
    }
  }
  return(FALSE)
}

hypersample <- function(FUN, ..., .maxit=10000) {
  modifyfun <- function(fun) {
    bod <- body(fun)
    if (trimws(as.character(bod[[1]])) == "{"){
      body(fun)[[2]] <- call("on.exit", quote( assign_funenv(as.list(environment()) )))
      if (length(bod) > 1) body(fun)[3:(1+length(bod))] <- bod[-1]
    } else {
      body(fun)[[1]] <- as.name('{')
      body(fun)[[2]] <- call("on.exit", quote( assign_funenv(as.list(environment()) )))
      body(fun)[[3]] <- bod
    }
    fun
  }
  #
  fun   <- modifyfun(match.fun(FUN))
  args  <- list(...)
  nargs <- names(args)
  if (is.null(nargs) || any('' %in% nargs)) stop("Unnamed parameter(s) found")
  largs <- lengths(args)
  .PVezKxhB3cKDVLbgWTQ996UDD0HRF <- list()
  browser()
  while (.maxit) {
    argsi <- list()
    for (i in seq_along(args)) {
      j <- sample.int(largs[i], 1)
      argsi[[nargs[i]]] <- args[[nargs[i]]][[j]]       
    }
    ret <- do.call(fun, argsi)
    if (all(ret)) break  
    .maxit <- .maxit-1
  }
  browser()
  stopifnot(.maxit>0)
  return(.PVezKxhB3cKDVLbgWTQ996UDD0HRF)
} 

hypergrid <- function(FUN, ..., .maxlen=10000) {
  browser()
  args  <- list(...)
  nargs <- names(args)
  if (is.null(nargs) || any('' %in% nargs)) stop("Unnamed parameter(s) found")
  largs <- as.list(lengths(args))
  for (i in 1:length(largs)) largs[[i]] <- 1:largs[[i]]
  index <- do.call(expand.grid, largs)
  param <- vector("list", nrow(index))
  browser()
  for(i in 1:nrow(index)) {
    for(j in 1:length(args)) {
      param[[i]][[j]] <- args[[j]][[index[i,j]]]
    }
    names(param[[i]]) <- names(args)
  }
  return(do.call(FUN, list(param)))
}

hypersample(x=list((0:10)/10), n=as.list(4:8), 
            FUN=function(x, n) {
              xi <- sample(x, size=n, replace=TRUE)
              m  <- mean(xi)
              s  <- sd(xi)
              is_terminal(c(m,s))
            })

ret <- hypergrid(n=as.list(5:100), p=as.list(seq(0.05, 0.95, by=0.05)), 
                 FUN=function(param) {
                   param <- lapply(param, function(e) {
                     np   <- e$n*e$p
                     npp  <- np*(1-e$p)            
                     snpp <- sqrt(npp)
                     keep <- is_terminal(snpp)
                     list(n=e$n, p=e$p, np=np, npp=npp, snpp=snpp, keep=is_terminal(snpp))
                   })
                   browser()
                   param <- do.call(rbind.data.frame, param)
                   param[param$keep,]
                 })

ret <- hypergrid(n=as.list(1:1000), p=as.list(seq(0.01, 0.99, by=0.01)), 
                 FUN=function(param) {
                   browser()
                   param <- lapply(param, function(e) {
                     np   <- e$n*e$p
                     npp  <- np*(1-e$p)            
                     snpp <- sqrt(npp)
                     norm <- (npp>=9)
                     pois <- (e$p<0.05) && (e$n>10)
                     sppn <- sqrt(e$p*(1-e$p)/e$n)
                     keep <- all(isTRUE(is_terminal(c(snpp, sppn))))
                     list(n=e$n, p=e$p, np=np, npp=npp, snpp=snpp, sppn=sppn, norm=norm, pois=pois, keep=is_terminal(snpp))
                   })
                   param <- do.call(rbind.data.frame, param)
                   param[param$keep,-ncol(param)]
                 }, .maxlen=1000000)

