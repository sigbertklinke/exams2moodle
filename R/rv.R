#' rv
#'
#' Formats a random variable and its meaning for R Markdown.
#'
#' @param symbol character: symbol
#' @param explanation character: meaning
#'
#' @return formatted string
#' @export
#'
#' @examples
#' rv("X", "Waiting time in minutes until next event")
rv <- function(symbol, explanation) {
  sprintf('$%s$: "%s"', symbol, explanation)
} 