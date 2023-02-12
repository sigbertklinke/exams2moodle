#' html_matrix_sk
#'
#' My personal pipe to create a \code{html_matrix} object. Note that the length of \code{fmt}
#' must be either \code{nrow(m)} or \code{ncol(m)} depending on \code{byrow}.
#' 
#' \preformatted{
#'    html_matrix(m) %>% hm_title(title) %>% zebra() %>%
#'      tooltip(sprintf(tooltip, nrow(m), ncol(m))) %>%
#'      hm_cell(fmt=fmt, byrow=byrow)
#' }
#'
#' @param m vector, matrix, array, table or html_matrix: input
#' @param title character: text for left upper entry
#' @param fmt character: format text for rows (or columns)
#' @param byrow logical: \code{fmt} by row or by column (default: \code{TRUE})
#' @param tooltip character: text for tooltip with column and row numbers (default: \code{"Die Tabelle hat \%.0f Zeilen und \%.0f Spalten"})
#' @param ... further parameters given to \code{html_matrix}
#'
#' @return html_matrix object
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' m <- matrix(1:6, ncol=2)
#' html_matrix_sk(m, title="", fmt=c("%.0f", "%.1f"))
html_matrix_sk <- function(m, title, fmt, byrow=TRUE, tooltip="Die Tabelle hat %.0f Zeilen und %.0f Spalten", ...) {
  if (byrow) stopifnot(length(fmt)==ncol(m)) else stopifnot(length(fmt)==nrow(m))
  html_matrix(m, ...) %>% hm_title(title) %>% zebra() %>%
    tooltip(sprintf(tooltip, nrow(m), ncol(m))) %>%
    hm_cell(fmt=fmt, byrow=byrow)
}
  