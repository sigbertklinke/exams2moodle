#' tooltip
#'
#' Adds a text tooltip to the HTML matrix.
#'
#' @param x htlm_matrix object
#' @param tooltip character: text to show (default: \code{NULL})
#'
#' @return html_matrix object
#' @export
#'
#' @examples
#' library("magrittr")
#' library("tools")
#' m    <- matrix(1:12, ncol=4)
#' hm   <- html_matrix(m) %>% 
#'           tooltip(sprintf("Table has %0.f rows and %0.f columns", nrow(.), ncol(.)))
#' html <- toHTML(hm, browser=TRUE)
tooltip <- function(x, tooltip=NULL) {
  attr(x, "tooltip") <- tooltip
  x
}