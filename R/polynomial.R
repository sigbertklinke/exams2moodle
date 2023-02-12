# Internal
polynomial2df <- function(p, digits=TRUE, decreasing=FALSE, tol=1e-9, simplify=TRUE) {
  oo <- options("scipen"=getOption('exams.scipen', 15))
  on.exit(options(oo))
  p       <- as.numeric(p)
  df      <- data.frame(p=p, x=abs(p))
  df$zero <- (df$x<tol)
  df$one  <- abs(df$x-1)<tol
  df$sign <- ifelse(p<0, "-", "+")
  if (is.logical(digits)) {
    if (digits) {
      df$val <- as.character(df$x)
    } else {
      f      <- fractions(df$x)
      df$val <- attr(f, "fracs")
    }
  }
  if (is.numeric(digits)) {
    df$val <- as.character(round(df$x, digits))
  }
  df$pow <- seq(nrow(df))-1
  if (decreasing) df <- df[nrow(df):1,]
  if (simplify) {
    # delete zero coefficients
    df <- df[!df$zero,]
    if (nrow(df)==0) { # no lines left
      df <- rbind(df, list(p=0, x=0, zero=TRUE, one=FALSE, sign="", val="0", pow=0))
    }    
    # delete one values
    df$val[df$one & df$pow] <- ""
    # delete first plus
    if (df$sign[1]=='+') df$sign[1] <- ''
  }
  df
}
  
#' toString.polynomial
#'
#' Creates text representation for a polynomial:
#' * if \code{digits} is \code{TRUE} then \code{as.character(.)} is used
#' * if \code{digits} is \code{FALSE} then \code{./.} is used
#' * if \code{digits} is numeric then \code{as.character(round(., digits))} is used
#' 
#' @param x polynomial: vector of coefficients (first is intercept) 
#' @param digits numeric or logical: how to convert to text (default: \code{NA})
#' @param decreasing logical:  order of the terms by increasing or decreasing powers (default:  \code{FALSE})
#' @param variable character: name of variable used (default: \code{"x"})
#' @param simplify logical: should the polynomial representation be simplified (default: `TRUE`)
#' @param tol numeric: tolerance (default: \code{1e-9}). A negative value will keep zeros and ones, too.
#' * If a coefficient is smaller than \code{tol} then zero terms are not kept.
#' * If a absolute value of coefficient minus one is smaller than \code{tol} then coefficient is not kept.
#' @param ... unused
#' @return a character 
#' @export
#'
#' @examples
#' p <- polynomial(c(-1,0,2)/3)
#' toString(p, 4)
#' toString(p, FALSE)
#' toString(p, TRUE)
#' toString(p, variable="z")
#' toString(p, decreasing=TRUE)
#' p <- polynomial(c(0,1,2)/3)
#' toString(p)
#' toString(p, tol=-1)
toString.polynomial <- function(x, digits=TRUE, decreasing = FALSE, variable="x", simplify=TRUE, tol=1e-9, ...) {
  df <- polynomial2df(x, digits=digits, decreasing=decreasing, simplify=simplify, tol=tol)
  paste0(df$sign, 
         df$val,
         ifelse((nchar(df$val)>0) & df$pow, '*', ''),
         ifelse(df$pow==0,      "", variable),
         ifelse(df$pow %in% c(0,1), "", paste0("^", df$pow)), collapse="")
}
  
#' toLatex.polynomial
#'
#' Returns a LaTeX representation of the polynomial.
#'
#' @param object polynomial
#' @param digits numeric or logical: how to convert to text (default: \code{NA})
#' @param decreasing logical:  order of the terms by increasing or decreasing powers (default:  \code{FALSE})
#' @param variable character: name of variable used (default: \code{"x"})
#' @param simplify logical: should the polynomial representation be simplified (default: `TRUE`)
#' @param tol numeric: tolerance (default: \code{1e-9}). A negative value will keep zeros and ones, too.
#' * If a coefficient is smaller than \code{tol} then zero terms are not kept.
#' * If a absolute value of coefficient minus one is smaller than \code{tol} then coefficient is not kept.
#' @param ... unused
#'
#' @return character
#' @export
#'
#' @examples
#' p <- polynomial(c(-1,0,2)/3)
#' toLatex(p, 4)
#' toLatex(p, FALSE)
#' toLatex(p, TRUE)
#' toLatex(p, variable="z")
#' toLatex(p, decreasing=TRUE)
#' p <- polynomial(c(0,1,2)/3)
#' toLatex(p)
#' toLatex(p, tol=-1)
toLatex.polynomial <- function(object, digits=TRUE, decreasing = FALSE, variable="x", simplify=TRUE, tol=1e-9, ...) {
  df <- polynomial2df(object, digits=digits, decreasing=decreasing, simplify=simplify, tol=tol)
  paste0(df$sign, 
         sapply(strsplit(df$val, '/', fixed=TRUE), 
                function(v) { if (length(v)==2) sprintf("\\frac{%s}{%s}", v[1], v[2]) else toString(v) }),
         ifelse((nchar(df$val)>0) & df$pow, '\\cdot ', ''),
         ifelse(df$pow==0,      "", variable),
         ifelse(df$pow %in% c(0,1), "", paste0("^", df$pow)), collapse="")
}


#' monomial
#'
#' Creates a polynomial of the form \eqn{c*x^d}.
#'
#' @param degree integer: degree of the polynomial (default: `1`)
#' @param coefficient numeric: coefficient of the polynomial (default: `1`)
#'
#' @return a poylnomial
#' @export
#'
#' @examples
#' monomial()     # equivalent to polynomial()
#' monomial(3)    # x^3
#' monomial(3, 2) # 2*x^3
monomial <- function(degree=1, coefficient=1) {
  stopifnot(degree>=0)
  polynomial(c(rep(0, ceiling(degree)), coefficient))
}