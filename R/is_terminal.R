#' is_terminal
#'
#' Checks whether all `x`'s can be expressed as a terminal fraction, 
#' which means the denominators are only dividable by two and five.
#'
#' @param x numeric: values to test
#'
#' @md
#' @return logical
#' @export
#'
#' @examples
#' is_terminal(2/3) # 0.6666... non-terminal
#' is_terminal(1/5) # 0.2       terminal
is_terminal <- function(x) {
  fracs <- if ('fractions' %in% class(x)) x else fractions(x)
  txt   <- strsplit(attr(fracs, "fracs"), "/", fixed=TRUE)
  denom <- sapply(txt, function(e) { if (length(e)>1) as.integer(e[2]) else 1})
  while(any(denom>1)) {
    sdenom <- sum(denom)
    denom  <- ifelse(denom%%2==0, denom/2, denom)
    denom  <- ifelse(denom%%5==0, denom/5, denom)
    if (sum(denom)==sdenom) return(all(denom==1))
  }
  return(TRUE) 
}