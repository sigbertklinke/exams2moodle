#' now
#'
#' Returns a time stamp based on the current time. 
#' If the clock resolution is not small enough then the function will throw an error.
#'
#' @param last integer: how many digits should be returned (default: \code{35})
#'
#' @return a character
#' @export
#'
#' @examples
#' now()   # return all digits
#' now(3)  # return only the first three digits
now <- function(last=35) {
  t1 <- gsub('.', '', sprintf("%.20f", as.numeric(Sys.time())), fixed=TRUE)
  i  <- .Machine$integer.max
  repeat{
    i  <- i-1
    t2 <- gsub('.', '', sprintf("%.20f", as.numeric(Sys.time())), fixed=TRUE)
    if(t1!=t2) break;
  }
  substr(t2, 1, last)
}