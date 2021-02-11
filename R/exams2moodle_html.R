#' exams2moodle_html
#'
#' Creates a HTML page with all list elements which names match \code{pattern}. Default is to match all list elements. 
#' If \code{html} is \code{NULL} then no file will be written; just the name of matched list elements will be returned invisibly.
#'
#' @param exam list: return list from \code{exam2moodle}
#' @param name character: name of HTML file (default: \code{NULL})
#' @param pattern character: string containing a regular expression to match list elements (default: \code{.})
#'
#' @return invisibly the names of list elements in the HTML file
#' @export
#'
#' @examples
#' x <- 1
exams2moodle_html <- function(exam, name=NULL, pattern=".") { 
  lst  <- unlist(exam)
  nlst <- names(lst)
  elem <- grepl(pattern, nlst)
  stopifnot(sum(elem)>0)
  lst  <- lst[elem]
  nlst <- names(lst)
  if (!is.null(name)) {
    ret  <- '<table>'
    for (i in 1:length(lst)) {
      ret <- c(ret, paste0('<tr><td bgcolor="grey" style="vertical-align: top;">',
                           gsub('.', ' ', nlst[i], fixed = TRUE),
                           '</td><td>',
                           lst[i],
                           '</td></tr>'))
    }
    ret <- c(ret, "</table>")
    if (all(!endsWith(name, c(".html", ".htm")))) name <- paste0(name, '_xml.html')
    writeLines(ret, name)
  }
  invisible(nlst)
}