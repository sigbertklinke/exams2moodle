#' unique_elem
#'
#' Deletes all elements from a hyperloop object that are identical. 
#' Since the result in each run can be a list itself, only specific list elements can be used for comparison.
#'
#' @param x hyperloop object
#' @param elem character: list elements which are used to check if hyperloop results are identical
#'
#' @return reduced hyperloop object
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' # 6 results: 3 different mu's, 2 var.equals
#' hl <- hyperloop(t.test, x=x, mu=list(-1, 0, 1), var.equal=list(TRUE, FALSE))
#' # reduction to 3 elements since var.equal does not play any role
#' length(unique_elem(hl))    
#' # reduction to 1 element since the mean of x always the same
#' length(unique_elem(hl, "estimate"))
unique_elem <- function(x, elem=NULL) {
  stopifnot("hyperloop" %in% class(x))
   ret  <- if (length(elem)) lapply(x, function(e) { e[elem] }) else x
   n    <- length(ret)
   keep <- rep(TRUE, n)
   for (i in 1:(n-1)) {
     for (j in (i+1):n) {
       if (keep[j] && identical(ret[[i]], ret[[j]])) {
         keep[j] <- FALSE
       }
     }
   }
   ret <- ret[keep]
   attr(ret, "loop") <- attr(x, "loop")[keep]
   class(ret) <- class(x)
   ret
}
                 