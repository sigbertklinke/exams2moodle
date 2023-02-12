#' @rdname toHTML
#' @aliases toLatex toHTMLorLatex
#' @title toHTML, toLatex, toHTMLorLatex
#' @description 
#' * `toHTML` returns a HTML representation of a matrix and optionally shows the result in the browser.
#' If you decide to view the result in a browser then the HTML will be written to a temporary file and 
#' [utils::browseURL()] called
#' * `toLatex` returns a LaTeX representation of a matrix, but supports just a small subset ot style options 
#' * `toHTMLorLatex` returns a HTML or LaTeX representation of a matrix, depending if `exams2pdf` is in the call list or not
#' 
#' @param x,object html_matrix object
#' @param browser logical: show HTML in a browser (default: \code{FALSE})
#' @param ... further parameters to [utils::browseURL()]
#'
#' @md
#' @return character
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
#' toLatex(hm)
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

#' @rdname toHTML
#' @export
toLatex.html_matrix <- function(object, ...) {
  style <- function(l, cont, colenv) {
    use <- setdiff(names(l), c("tooltip", "value", "fmt", "")) 
    ret <- cont
    if (length(use)==0) return(ret)
    mc <- ''
    if (!is.null(l$background_color)) {
      col <- l$background_color
      if (startsWith(col, '#')) {
        col <- toupper(substring(col, 2))
        hmc <- colenv$hmc
        pos <- which(hmc$html==col)
        if (length(pos)==0) {
          pos <- min(which(hmc$html==''))
          hmc[pos, 'html'] <- col
          colenv$hmc <- hmc
        }
        col <- hmc[pos, 'name']
      } 
      mc <- paste0('>{\\columncolor{', col, '}}')
    }   
    if(!is.null(l$text_align)) mc <- paste0(mc, substr(l$text_align, 1, 1))
    if (!is.null(l$font_weight)) ret <- paste0("\\textbf{", ret, '}')
    paste0("\\multicolumn{1}{", mc, '}{', ret, '}')    
  }
  #
  x <- object
  stopifnot("html_matrix" %in% class(x))
#  tooltip <- attr(x, "tooltip")
#  tabletitle <- if (is.null(tooltip)) '' else sprintf(' title="%s"', tooltip)
  colors <- new.env()
  index  <- 0:675
  colors$hmc <- data.frame(name=paste0("htmlmatrix", LETTERS[1+(index%/%26)], LETTERS[1+(index%%26)]),
                           html=rep('', length(index)))
  latex <- c("\\begin{table}[h]", '\\centering', paste0("\\begin{tabular}{", paste0(rep('c', ncol(x)+1), collapse=""), '}'))
#  tr    <- attr(x, "tr")
  title <- attr(x, "title")
  rows  <- attr(x, "rownames")
  cols  <- attr(x, "colnames")  
  for (r in 0:nrow(x)) {
#    latex <- paste0(latex, "<tr", style(tr[[r+1]]), ">")
    lrow <- ''
    for (c in 0:ncol(x)) {
      if (r) {
        if (c) {
          lrow <- paste0(lrow, ' & ', style(x[[r,c]], sprintf(x[[r,c]]$fmt, x[[r,c]]$value), colors))
        } else {
          lrow <- paste0(lrow, style(rows[[r]], sprintf(rows[[r]]$fmt, rows[[r]]$value), colors))       
        }
      } else {
        if (c) {
          lrow <- paste0(lrow, ' & ', style(cols[[c]], sprintf(cols[[c]]$fmt, cols[[c]]$value), colors))   
        } else {
          lrow <- paste0(lrow, style(title, sprintf(title$fmt, title$value), colors))
        }
      }
    }
    latex <- c(latex, paste0(lrow, "\\\\"))
  }
  latex <- c(latex, "\\end{tabular}", "\\end{table}") 
  index <- which(colors$hmc[,'html']!='')
  for (i in seq_along(index)) {
    latex <- c(sprintf("\\definecolor{%s}{HTML}{%s}", colors$hmc[i,'name'], colors$hmc[i,'html']), latex)
  }
  paste0(latex, collapse="\n") 
}

#' @rdname toHTML
#' @export
toHTMLorLatex <- function(x, ...) {
  stopifnot("html_matrix" %in% class(x))
  if (calledBy('exams2pdf')) toLatex(x) else toHTML(x) 
}