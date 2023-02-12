#' inline
#'
#' Knits `txt` within a R code chunk.
#' 
#' @param txt character
#'
#' @return just output
#' @importFrom knitr knit knit_global
#' @export
#'
#' @examples
#' 1+1
inline <- function(txt) {
  cat(knitr::knit(text=txt, envir = knitr::knit_global(), quiet=TRUE))
}