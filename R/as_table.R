#' as_table
#'
#' Converts a vector into a horizontal table. 
#'
#' @inheritParams xtable::xtable
#' @param ... further parameters for \code{print.xtable}
#'
#' @return string
#' @importFrom xtable xtable print.xtable
#' @export
#'
#' @examples
#' x <- runif(5)
#' tab <- vec2mat(x, colnames=1:length(x))
#' as_table(tab)
as_table <- function(x, caption = NULL, label = NULL, align = NULL, digits = NULL,
                     display = NULL, auto = FALSE, ...) {
  xt <- xtable(x, caption = caption, label = label, align = align, digits = digits,
               display = display, auto = auto)
  xt <- print(xt, ...)
  strsplit(xt, "\n", fixed=TRUE)[[1]]
}
