#' ptext
#'
#' Creates text representation for a polynom:
#' * if \code{digits} is \code{NA} then \code{as.character(.)} is used
#' * if \code{digits} is numeric then \code{as.character(round(., digits))} is used
#' * if \code{digits} is \code{TRUE} then \code{\\frac{.}{.}} is used
#' * if \code{digits} is \code{FALSE} then \code{./.} is used
#' 
#' @md
#' @param p polynomial: vector of coefficients (first is intercept) 
#' @param digits numeric or logical: how to convert to text (default: \code{NA})
#' @param decreasing logical:  order of the terms by increasing or decreasing powers (default:  \code{FALSE})
#' @param variable character: name of variable used (default: \code{"x"})
#' @param tol numeric: tolerance (default: \code{1e-9}). A negative value will keep zeros and ones, too.
#' * If a coefficient is smaller than \code{tol} then zero terms are not kept.
#' * If a absolute value of coefficient minus one is smaller than \code{tol} then coefficient is not kept.
#' 
#' @return a character 
#' @export
#'
#' @examples
#' #library("polynom")
#' p <- polynomial(c(-1,0,2)/3)
#' ptext(p, 4)
#' ptext(p, FALSE)
#' ptext(p, TRUE)
#' ptext(p, variable="z")
#' ptext(p, decreasing=TRUE)
#' p <- polynomial(0, 1, 2/3)
#' ptext(p)
#' ptext(p, tol=-1)
ptext <- function(p, digits=NA, decreasing = FALSE, variable="x", tol=1e-9) {
  pfrac <- function(v) {
    if (length(v)==1) return(v)
    paste0("\\frac{", v[1], "}{", v[2], "}")    
  }
  #
  if (!inherits(p, "polynomial")) p <- polynomial(p)
  #
  pn      <- as.numeric(p)
  df      <- data.frame(p=pn, x=abs(pn))
  df$zero <- (df$x<tol)
  df$one  <- abs(df$x-1)<tol
  df$sign <- ifelse(pn<0, "-", "")
  if (is.na(digits)) {
    df$val <- as.character(df$x)
  } else {
    if (is.logical(digits)) {
      f     <- fractions(df$x)
      df$val <- attr(f, "fracs")
      if (digits) df$val <- sapply(strsplit(df$val, "/"), pfrac)
    } else {
      df$val <- as.character(round(df$x, digits))
    }
  }
  df$pow <- seq(nrow(df))-1
  if (decreasing) df <- df[nrow(df):1,]
  # delete zero coefficients
  df <- df[!df$zero,]
  if (nrow(df)==0) { # no lines left
    df <- rbind(df, list(p=0, x=0, zero=TRUE, one=FALSE, sign="", val="0", pow=0))
  }
  # delete one values
  df$val[df$one & df$pow] <- ""
  # make string
  ret <- paste0(df$sign, 
                df$val,
                ifelse(df$pow==0,      "", variable),
                ifelse(df$pow %in% c(0,1), "", paste0("^", df$pow)))
  gsub("+-", "-", paste0(ret, collapse="+"), fixed=TRUE)
}