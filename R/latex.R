#' @rdname latex_helper
#' @title Supporting functions for math LaTeX output
#' @description \code{lsumprod} creates a latex printout of \eqn{\sum_i x_i y_i} with brackets if \eqn{x_i} or \eqn{y_i} starts with a `-`.
#' 
#' @param x numeric: input values
#' @param mu numeric: population mean (default: `NULL`)
#' @param br character: which brackets to use, either 
#' * `(` (default) uses `\left(` and `\right(`, 
#' * `[` use `\left[` and `\right]`, 
#' * \code{\{} use `\left\{` and `\right\}`,
#' * `|`  use `\left|` and `\right|`,
#' * `||`  uses `\left\|` and `\right\|`,
#' * `<`, `a` use `\left\langle` and `\right\rangle`,
#' * `c`  use `\left\lceil` and `\right\rceil`, and
#' * `f`  use `\left\lfloor` and `\right\rfloor`. 
#' @param subset logical: indicates to which elements brackets are added (default: `NULL` = all elements statrting with `-`); missing values are taken as false.
#' @param ... further input values
#' 
#' @return character
#' @export
#'
#' @examples
#' lsumprod(-2:2, (1:5)/10)
#' lbr(-2:2)
#' lsum(-2:2)
#' lmean(-2:2)
lsumprod <- function(..., br="(") {
  args <- list(...)
  stopifnot(length(args)>1)
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  ret <- lbr(args[[1]])
  for (i in 2:length(args)) ret <- paste(ret, '\\cdot', lbr(args[[i]], br))
  paste0(ret, collapse=" + ")
}

#' @rdname latex_helper 
#' @description \code{lsum} creates a latex printout of \eqn{x} as sum.
#' @export 
lsum <- function(x) {
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  paste0(c(x[1], lsgn(x[-1])), collapse="")
}

#' @rdname latex_helper 
#' @description \code{lprod} creates a latex printout of \eqn{x} as product.
#' @export 
lprod <- function(x) {
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  paste0(lbr(x), collapse=" \\cdot ")
}


#' @rdname latex_helper 
#' @description \code{lmean} creates a latex printout as \eqn{\frac{x_1+...+x_n}{n}}.
#' @export 
lmean <- function(x) {
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  paste0("\\frac{", lsum(x), "}{", as.character(length(x)), "}")
}

#' @rdname latex_helper 
#' @description \code{lvar} creates a latex printout as \eqn{\frac{(x_1-xbar)^2+...+(x_n-xbar)^2}{n}}.
#' @export 
lvar <- function(x, mu=NULL, br="(") {
  oo  <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  if (is.null(mu)) {
    n  <- length(x)-1
    xm <- lbr(mean(x))
  } else {
    n  <- length(x)
    xm <- lbr(mu)
  }
  ret <- paste0("\\left(", x, "-", xm, "\\right)^2", collapse=" + ") 
  paste0("\\frac{", ret, "}{", n, "}")
}

#' @rdname latex_helper 
#' @description \code{lbr} creates a latex printout of \eqn{x} with brackets if \eqn{x} starts with a `-`.
#' @export 
lbr <- function(x, br=c("(", "[", "{", "|", "||", "<", "a", "c", "f"), subset=NULL) {
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  bracket <- match.arg(br)
  ret <- trimws(as.character(x))
  if (is.null(subset)) subset <- startsWith(ret, "-") else rep_len(subset, length(x))
  subset <- !is.na(subset) & subset
  paste0(ifelse(subset, pkgenv$opening[bracket], ""), ret, ifelse(subset, pkgenv$closing[bracket], ""))  
}

#' @rdname latex_helper 
#' @description \code{lsgn} creates a latex printout of \eqn{x} with a plus or minus at the beginning.
#' @export 
lsgn <- function(x) {
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  ret <- trimws(as.character(x))
  minus <- startsWith(ret, '-')
  paste0(ifelse(minus, '', '+'), ret)  
}


#' @rdname latexdef
#' @title latexdef
#' @aliases answercol
#' @description If exams is called by `exams2pdf` 
#' * `latex1def` adds a `\def\name{body}`, and
#' * `answercol` adds a `\def\answercol{n}` 
#' to the LaTeX file.
#' @param name character: macro name
#' @param body,n character: macro body 
#'
#' @return nothing
#' @export
#'
#' @examples
#' answercol(2)
latexdef <- function(name, body) {
  stopifnot(all(unlist(strsplit(name, '')) %in% c(letters, LETTERS)))
  if (calledBy("exams2pdf")) writeLines(c(sprintf("\\def\\%s{%s}", name, as.character(body)), "")) 
}

#' @rdname latexdef
#' @export
answercol <- function(n) { 
  stopifnot(n>1)
  latexdef("answercol", n) 
} 