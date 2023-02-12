#' hyperloop
#' 
#' Runs a function several times with all parameter combinations:
#' 
#' * If an argument is not a list then it will be converted to a one element list.
#' * If an error occurs then the result of \code{FUN} will not be stored-
#'
#' @param FUN function with named parameter(s)
#' @param ... named parameters which contain lists with possible parameter values
#' @param .simplify logical: should the result be simplified to a data frame if possible? (default: `FALSE`)
#'
#' @return a hyperloop object as list
#' @export
#'
#' @examples
#' x   <- rnorm(100)
#' trm <- hyperloop(mean, x=list(x), trim=as.list(seq(0, 0.5, by=0.05)))
#' # automatic conversion of x to list(x)
#' trm <- hyperloop(mean, x=x, trim=as.list(seq(0, 0.5, by=0.05))) 
#' unlist(trm)
hyperloop <- function(FUN, ..., .simplify=FALSE) {
  args       <- list(...)
  for (i in 1:length(args)) {
    if (!is.list(args[[i]])) args[[i]] <- list(args[[i]])
  }
  args$FUN       <- FUN
  args$.simplify <- FALSE
  ret <- do.call(gapply, args)
  if (.simplify) return(gsimplify(ret))
  ret
#  args       <- list(...)
#  stopifnot(!is.null(names(args)))
#  stopifnot(all(names(args)!=""))
#  for (i in 1:length(args)) {
#    if (!is.list(args[[i]])) args[[i]] <- list(args[[i]])
#  }
#  hyperindex <- lapply(args, function(e) { 1:length(e) } )
#  loop       <- expand.grid(hyperindex)
#  ret        <- vector("list", nrow(loop))
#  rloop      <- vector("list", nrow(loop))
#  keep       <- rep(TRUE, nrow(loop))
#  for (i in 1:nrow(loop)) {
#    argsi <- list()
#    for (j in 1:length(args)) {
#      argsi[[j]] <- args[[j]][[loop[i,j]]]
#    }
#    names(argsi) <- names(args)
#    tryCatch({ret[[i]]   <- do.call(FUN, argsi) }, 
#             error = function(e) { ret[[i]] <- NULL; keep[i] <- FALSE})
#    rloop[[i]] <- loop[i,]
#  }
#  ret   <- ret[keep]
#  rloop <- rloop[keep]
#  structure(ret, loop=rloop, class="hyperloop")
}

#' gsimplify 
#'
#' Simplifies a `hyperloop` object if possible.
#'
#' @param ga list: `hyperloop` object
#' @param exclude character or integer: elements to exclude in each list element of `ga` (default: `NULL`)
#' @param subset indices specifying elements of `ga` to extract (default: `NULL`)
#'
#' @return a data frame if possible otherwise a list
#' @export
#'
#' @examples
#' # calls: t.test(x, -1), t.test(x, 0), t.test(x, 1)
#' ga <- gapply(t.test, x=I(rnorm(100)), mu=-1:1)
#' # no simplication since `data.name` and `conf.int` have lengths larger  than one
#' gsimplify(ga)
#' #' now simplication possible
#' gsimplify(ga, exclude=c("conf.int", "data.name"))
gsimplify <- function(ga, exclude=NULL, subset=NULL) {
  stopifnot(is.list(ga))
  if (!is.null(subset)) ga <- ga[subset]
  index <- 1:length(ga[[1]])
  if (!is.null(exclude)) {
    if (is.numeric(exclude)) {
      index <- setdiff(index, exclude) 
    } 
    if (is.character(exclude)) {
      index <- setdiff(names(ga[[1]]), exclude)
    }
  }
  lone <- rep(FALSE, length(ga))
  ret  <- vector("list", length(ga))
  for (i in 1:length(ga)) {
    ret[[i]] <- ga[[i]][index]    
    lone[i] <- ((max(lengths(ret[[i]])))<2)
  }
  if (all(lone)) {
    nret <- if(is.null(names(ret[[1]]))) nret <- rep('', length(ret[[1]])) else names(ret[[1]])
    ret  <- do.call(rbind.data.frame, ret)
    names(ret) <- ifelse(nret=='', sprintf("V%i", 1:length(nret)), nret) 
    ret <- as.data.frame(ret)
  }
  structure(ret, class=unique(c("hyperloop", class(ret))))
}

#' gapply
#'
#' Runs all combinations of elements in `...` as parameters of `FUN` (grid apply). 
#' `I(.)` can be used to avoid that an element is interpreted as a grid value.
#' If an error occurs then the result of \code{FUN} will not be stored, you may note missing indices
#' in the returning list.
#'
#' @param FUN fucntion or character: string naming the function to be called
#' @param ... list: arguments to the function call. The names attribute of args gives the argument names
#' @param .simplify logical: should the result be simplified to a data frame if possible? (default: `TRUE`)
#'
#' @return a list or a data frame with the function results
#' @export  
#'
#' @examples
#' # 8 function calls: sum(1,3,5), sum(1,3,6), ..., sum(2,4,6)
#' gapply("sum", 1:2, 3:4, 5:6)
#' # 4 function calls: sum(1,3,5:6), sum(1,4,5:66), ..., sum(2,4,5:6)
#' gapply("sum", 1:2, 3:4, I(5:6))
gapply <- function(FUN, ..., .simplify=TRUE) {
  args <- list(...)  
  nargs <- lengths(args)
  gargs <- ifelse(sapply(args, function(e) { "AsIs" %in% class(e) }), 0, lengths(args))
  index <- lapply(1:length(gargs), function(i) { if (gargs[i]==0) return(0); 1:gargs[i]})
  index <- do.call(base::expand.grid, index)
  ret   <- list() 
  for (i in 1:nrow(index)) {
    farg <- list()
    for (j in 1:ncol(index)) {
      if (index[i,j]==0) {
        farg[[j]] <- args[[j]]
      } else {
        farg[[j]] <- if (is.list(args[[j]])) args[[j]][[index[i,j]]] else args[[j]][index[i,j]]
      }                                           
    }
    names(farg) <- names(args)
    reti <- try(do.call(FUN, farg), silent=TRUE)
    ret[[i]] <- if ('try-error' %in% class(reti)) NULL else reti
  }
  if (.simplify) ret <- gsimplify(ret)
  structure(ret, class=unique(c("hyperloop", class(ret))))
}