#' moodle_text2html
#'
#' Extracts certain \code{<text>...</text>} elements from the XML file and creates an HTML file from it.
#'
#' @param xml character: name of XML file
#' @param html character: name of HTML file (default: \code{sub(".xml$", "_text.html", xml)} )
#' @param elem character: text elements to put in the HTML file  (default: \code{NULL} = all elements)
#'
#' @return invisibley the HTML file name
#' @export
#'
#' @examples
#' x <- 1
moodle_text2html <- function(xml, html=sub(".xml$", "_text.html", xml), elem=NULL) {
  stopifnot(xml!=html)
  xml <- moodle_xml2list(xml)
  txt <- moodle_text(xml)
  out <- ''
  for (i in 1:length(txt)) {
    out <- c(out, paste0('<h3>', i, '</h3>\n<table>'))
    for (name in names(txt[[i]])) {
      if (is.null(elem) || (name %in% elem)) {
        out <- c(out, paste0('<tr><td bgcolor="grey" style="vertical-align: top;">', name, '</td><td>', txt[[i]][[name]], '</td></tr>'))
      }
    }
    out <- c(out, '</table>\n\n')  
  }
  writeLines(out, html)
  invisible(html)
}