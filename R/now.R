#' now
#' 
#' Returns a time stamp based on the current time. \code{now} calls basically 
#' \code{gsub('.', '', sprintf('\%.20f', as.numeric(Sys.time())), fixed=TRUE)}. 
#' To ensure that at each call a different time stamp is delivered \code{now} 
#' may call \code{gsub(...)} several times until two different results are delivered. 
#' The last one is then returned.
#'
#' @param last integer: how many digits should be returned (default: \code{35})
#'
#' @return a character
#' @export
#'
#' @examples
#' now()   # returns all digits
#' now(3)  # returns only the first three digits
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