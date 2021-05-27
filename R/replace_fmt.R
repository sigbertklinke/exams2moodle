#' replace
#'
#' Replaces in a text names with 
#' * values which are formatted with [exams::fmt()], or
#' * strings.
#'
#' @param txt character: text where replacment is done 
#' @param digits numeric or list: number of digits to round 
#' @param ... names to replace with values
#'
#' @return character with replaced names
#' @export
#'
#' @examples
#' replace_fmt("\\frac{x}{y}", x=2, y=3)
#' replace_fmt("\\frac{x}{y}", x=2, y=3, digits=0)
#' replace_fmt("\\frac{x}{y}", x=2, y=3, digits=list(0))
#' replace_fmt("\\frac{x}{y}", x=2, y=3, digits=list(2, y=0))
#' replace_fmt("\\frac{x}{y}", x="\\\\sum_{i=1}^n x_i", y="\\\\sum_{i=1}^n y_i")
replace_fmt <- function(txt, digits = 2L, ...) {
  args  <- list(...)
  nargs <- names(args)
  if(is.null(nargs)) return(txt)
  #browser()
  if (is.list(digits)) {
    if (is.null(names(digits))) {
      dd <- digits[[1]]
    } else {
      ind <- which(names(digits)=="")
      dd <- if (length(ind)) args[[ind[1]]] else 2 
    }
  } else {
    dd <- digits
  }
  for (i in seq(nargs)) {
    if (nchar(nargs[i])) {
      if (is.numeric(args[[i]])) {
        di <- dd
        if (is.list(digits)) {
          if (!is.null(digits[[nargs[i]]])) di <- digits[[nargs[i]]]
        } 
        txt <- gsub(paste0("\\b", nargs[i], "\\b"), fmt(args[[i]], di), txt)
      } 
      if (is.character(args[[i]])) {
        txt <- gsub(paste0("\\b", nargs[i], "\\b"), args[[i]], txt)
      } 
    }
  }
  txt
}