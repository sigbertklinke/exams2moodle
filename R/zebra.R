#' zebra
#'
#' @param x html_matrix object
#' @param col a vector of colors to zebra with (default:\code{c("#FFFFFF", "#CCCCCC")})
#' @param byrow logical: zebra by row or by column (default: \code{TRUE})
#'
#' @return html_matrix object
#' @export
#'
#' @examples
#' library("magrittr")
#' library("tools")
#' m    <- matrix(1:12, ncol=4)
#' hm   <- html_matrix(m) %>% zebra()
#' html <- toHTML(hm, browser=TRUE)
zebra <- function(x, col=c("#FFFFFF", "#CCCCCC"), byrow=TRUE) {
  stopifnot("html_matrix" %in% class(x))
  if (byrow) {
    col <- rep(col, length.out=nrow(x))    
    for (i in 1:nrow(x)) {
      for (j in 1:ncol(x)) {
        x[[i,j]]$background_color <- col[i] 
      }
    }
  } else {
    col <- rep(col, length.out=ncol(x))    
    for (i in 1:nrow(x)) {
      for (j in 1:ncol(x)) {
        x[[i, j]]$background_color <- col[j] 
      }
    }  
  }
  x
}
