mathformula <- function(x, ...) { UseMethod('mathformula') }

subexpressions <- function(pd) {
  # version 1
  l1 <- unique(setdiff(pd$text[pd$token=="expr" & !pd$terminal],
                       pd$text[pd$token %in% c("SYMBOL_FUNCTION_CALL", "NUM_CONST")]))
  #
  l <- l1
  names(l) <- c(sprintf("%i.1", 1:length(l1)))
  l
}

mathformula.default <- function(x, ...) {
  mf <- getParseData(parse(text=x), TRUE)
  structure(list(parsedata=mf, sub=subexpressions(mf), text=x), class="mathformula")
}

print.mathformula <- function(x, ...) {
  cat(x$text, "\n")
}

summary.mathformula <- function(object, ...) {
  data.frame(expression=object$sub)
}

supersede <- function(.formula, ...) {
  stopifnot(inherits(.formula, 'mathformula'))
  args    <- list(...)
  nargs   <- names(args)
  if ((length(args)==0) || is.null(nargs)) return(.formula)
  #
  istext <- is.na(suppressWarnings(as.numeric(nargs)))
  nargs  <- ifelse(istext, .formula$sub[pmatch(nargs, .formula$sub)], .formula$sub[nargs])
  rdata  <- unique(.formula$parsedata[!is.na(match(.formula$parsedata$text, nargs)), c('col1', 'col2', 'text'),drop=FALSE])
  txt    <- .formula$text
  if (nrow(rdata)) {
    names(args) <- nargs
    # do pattern overlap?
    stopifnot(anyDuplicated(unlist(apply(rdata, 1, function(r) { r['col1']:r['col2'] })))==0)
    o <- order(rdata$col1, decreasing=TRUE)
    for (i in o) {
     txt <- paste0(substr(txt, 1, rdata[i,1]-1), args[[rdata[i,3]]], substring(txt, rdata[i,2]+1))
    }
  }
  mathformula(txt)
}

toLatex <- function(object, ...) {
  object <- supersede(object, ...)
  #
  env  <- new.env()
  env[["frac"]] <- function(x,y) { paste0("\\frac{", x, "}{", y , "}") }
  env[["sum"]]  <- function(i, f, t, e) { paste0("\\sum_{", i, "=", f, "}^{", t, "}", e )}
  env[["mean"]] <- function(x) { paste0("\bar{", x, "}") }
  env[["["]]    <- function(obj, ind) { structure(paste0(obj, "_{", ind, "}"), simple=isTRUE(attr(obj, 'simple'))) }
  env[["-"]]    <- function(l, r) { paste0(l, "-", r) }
  env[["^"]]    <- function(l, r) { if (isTRUE(attr(l, 'simple'))) paste0(l, "^{", r, "}") else paste0("(", l, ")^{", r, "}") }
  env[["*"]]    <- function(l, r) { paste0(l, "\\cdot", r) }
  #
  expr <- parse(text=object$text)
  vars <- all.vars(expr)
  for (v in vars) env[[v]] <- structure(v, simple=TRUE)
  eval(expr, envir=env)
}

decomposeArgs <- function(...) {
  args  <- list(...)
  nargs <- names(args)
  if (is.null(args)) nargs <- rep('', length(args))
  zero <- (nchar(nargs)==0)
  list(named=args[!zero], unnamed=args[zero])
}

expand <- function(.formula, ...) {
  args <- decompseArgs(...)
  args$named$.formula <- .formula
  formula <- do.call(supersede, args$named)
  .expand <- if (length(args$unnamed)==0) rev(.formula$sub) else unlist(args$unnamed)
    

    
  mathformula(.formula$text)
}

latex1 <- mathformula("frac(1,n)*sum(i,1,n,(x[i]-mean(x))^2)")
latex2 <- "frac(sum(i,1,n, (x[i]-mean(x))^2), n)"  
latex3 <- "sum(i,1,n, x_i^2)"
toLatex(latex1)
toLatex(latex1, n=10, '11.1'=2.2)


parseTree <- function(cmd) {
  buildtree <- function(line, pd) {
    id   <- pd[line, 'id']
    ret  <- list()
    line <- which(pd$parent==id) 
    for (i in seq_along(line)) {
      ret[[i]] <- if (pd[line[i],'terminal']) structure(pd[line[i], 'text'], token=pd[line[i], 'token']) else buildtree(line[i], pd)
    }
    if (length(ret)==1) return(ret[[1]])
    ret
  }
  #
  simplifytree <- function(tree) {
    ret   <- list()
    text  <- NULL
    token <- NULL
    nret  <- 1
    for (i in 1: length(tree)) {
      if (length(tree[[i]])==1) {
        text  <- c(text, tree[[i]])
        token <- c(token, attr(tree[[i]], "token"))
      } else {
        if (length(text)) {
          ret[[nret]] <- structure(text, token=token)
          text  <- NULL
          token <- NULL
          nret  <- nret+1
        }
        ret[[nret]] <- simplifytree(tree[[i]])  
        nret <- nret+1
      }
    }
    if (length(text)) {
      ret[[nret]] <- structure(text, token=token)
      text  <- NULL
      token <- NULL
      nret  <- nret+1
    }
    ret
  }
  #
  e <- parse(text=cmd)
  pd <- getParseData(e)
  #
  tree <- buildtree(1, pd)
  tree <- simplifytree(tree)
  tree
}

t<-parseTree(latex1)


# S3
sort(unique(c(Filter(isGeneric,ls(all.names=TRUE, env = baseenv())),
              unlist(Filter(length,sapply(search(), function(x) {
                Filter(isGeneric,ls(all.names=TRUE,env = as.environment(x)))
              } ))),
              .knownS3Generics)
))
              