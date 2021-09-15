#' toHTML
#'
#' Returns a HTMl representation of a matrix and optionally shows the result in the browser.
#' If you decide to view the result in a browser then the HTML will be written to a temporary file and 
#' [utils::browseURL()] called
#' 
#' @param x html_matrix object
#' @param browser logical: show HTML in a browser (default: \code{FALSE})
#' @param ... further parameters to [utils::browseURL()]
#'
#' @md
#' @return html_matrix object
#' @importFrom utils browseURL
#' @importFrom tools toHTML
#' @export
#'
#' @examples
#' library("tools")
#' m    <- matrix(1:12, ncol=4)
#' hm   <- html_matrix(m)
#' html <- toHTML(hm, browser=TRUE)
#' toHTML(hm)
toHTML.html_matrix <- function(x, browser=FALSE, ...)  {
    style <- function(l) {
    use <- setdiff(names(l), c("tooltip", "value", "fmt", "")) 
    if (length(use)==0) return('')
    use2 <- gsub("_", "-", use, fixed=TRUE)
    txt <- ' style="'
    for (k in seq(use2)) {
      txt <- paste0(txt, paste0(use2[k], ':', as.character(l[[use[k]]]), ';'))
    }
    paste0(txt, '"')
  }
  #
  stopifnot("html_matrix" %in% class(x))
  tooltip <- attr(x, "tooltip")
  tabletitle <- if (is.null(tooltip)) '' else sprintf(' title="%s"', tooltip)
  html <- paste0("<table", style(attr(x, "table")), tabletitle, ">\n")
  tr    <- attr(x, "tr")
  title <- attr(x, "title")
  rows  <- attr(x, "rownames")
  cols  <- attr(x, "colnames")  
  for (r in 0:nrow(x)) {
    html <- paste0(html, "<tr", style(tr[[r+1]]), ">")
    for (c in 0:ncol(x)) {
      if (r) {
        if (c) {
          html <- paste0(html, "<td", style(x[[r,c]]), ">", sprintf(x[[r,c]]$fmt, x[[r,c]]$value), "</td>")  
        } else {
          html <- paste0(html, "<td", style(rows[[r]]), ">", sprintf(rows[[r]]$fmt, rows[[r]]$value), "</td>")       
        }
      } else {
        if (c) {
          html <- paste0(html, "<td", style(cols[[c]]), ">", sprintf(cols[[c]]$fmt, cols[[c]]$value), "</td>")              
        } else {
          html <- paste0(html, "<td", style(title), ">", sprintf(title$fmt, title$value), "</td>")  
        }
      }
    }
    html <- paste0(html, "</tr>\n")
  }
  html <- paste0(html, "</table>") 
  if (browser) {
    file <- tempfile(fileext=".html")
    writeLines(html, file)
    browseURL(file, ...)  
  }
  html
}
