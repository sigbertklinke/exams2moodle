#' makekey
#'
#' generates a character key from a vector of integers
#'
#' @param index integer: vector of integer
#'
#' @return a character
#' @export
#'
#' @examples
#' makekey(1)
#' makekey(1:2)
#' makekey(pi) # ;)
#' makekey(c(5,4))
makekey <- function(index) { 
  toString(as.integer(index)) 
}