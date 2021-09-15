#' print
#' 
#' print a HTML matrix content or its components 
#' 
#' @param x html_matrix object
#' @param ... further parameters, unused
#' @param which character: which component to print (default: \code{""})
#'
#' @return invisible the text matrix shown
#' @export
#'
#' @examples
#' m <- matrix(1:6, ncol=2)
#' l <- html_matrix(m)
#' print(l, which=NA)      # returns full style information
#' print(l, which="fmt")   # returns format information
#' print(l, which="value") # identical to print(l)
print.html_matrix <- function(x, ..., which="") {
  entry <- function(l, which) {
    ret <- ''
    if (is.na(which)) {
      elems <- setdiff(names(l), c("fmt", "value"))
      for (elem in elems) {
        ret <- paste0(ret, elem, ':', l[[elem]], ';')
      }      
    } else if (which=="") {
      if (!is.null(l$value)) {
        ret <- if (is.null(l$fmt)) as.character(l$value) else sprintf(l$fmt, l$value)
      }
    } else {
      if (!is.null(l[[which]])) ret <- as.character(l[[which]])
    }
    ret
  }
  #
  m <- matrix('', ncol=ncol(x)+1, nrow=nrow(x)+1) 
  n <- matrix(NA_integer_, ncol=ncol(x)+1, nrow=nrow(x)+1)
  m[1,1] <- entry(attr(x, "title"), "value")
  for (i in 1:nrow(x)) {
    m[i+1,1] <- entry(attr(x, "rownames")[[i]], which)
    for (j in 1:ncol(x)) {
      m[i+1, j+1] <- entry(x[[i,j]], which)
    }
  }
  for (j in 1:ncol(x)) {
    m[1, j+1] <- entry(attr(x, "colnames")[[j]], which)
  }
  fmt <- paste0("%", apply(nchar(m), 2, max), "s ")
  for (i in 1:nrow(m)) {
    cat("\n")
    for (j in 1:ncol(m)) cat(sprintf(fmt[j], m[i,j]))
  }
  cat("\n")
  invisible(m)
}
