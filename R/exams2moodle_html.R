#' exams2moodle_html
#'
#' Creates a HTML page with all list elements which names match \code{pattern}. Default is to match all list elements. 
#' If \code{html} is \code{NULL} then no file will be written; just the name of matched list elements will be returned invisibly.
#' If elements are merged then they are collapsed with \code{"\n"}. A named list of the form \code{"pattern"=collapse} 
#' can be used to modify the character string to separate elements, e.g. by using \code{"<br>"}.
#'
#' @param exam list: return list from \code{exam2moodle}
#' @param name character: name of HTML file (default: \code{NULL})
#' @param pattern character: string containing a regular expression to match list elements (default: \code{.})
#' @param mathjax logical: should be MathJax loaded (default: \code{TRUE})
#' @param header integer: at which level of the list a \code{<h2>...</h2>} element should be included (default: \code{2})
#' @param merge list: should elements with \code{.XXXXnn} at the end merged (default: \code{list('questionlist'="<br>")})
#'
#' @return invisibly the names of list elements in the HTML file
#' @export
#'
#' @examples
#' x <- 1
exams2moodle_html <- function(exam, name=NULL, pattern=".", mathjax=TRUE, 
                              header=2, merge=list('questionlist'="<br>")) { 
  lst  <- unlist(exam)
  nlst <- names(lst)
  if (!is.null(merge)) {
    nlst  <- gsub('[0-9]+$', '', nlst)
    index <- structure(1:length(nlst), names=nlst)
    plst  <- names(merge)
    lst   <- tapply(index, nlst, function(ind) { 
      if (length(ind)<2) return(lst[ind])
      for (i in 1:length(plst)) {
        if (grepl(plst[i], nlst[ind[1]])) {
          return(paste0(lst[ind], collapse=merge[[i]]))          
        }
      }
      paste0(lst[ind], collapse="\n")
    })
    nlst  <- names(lst)  
  }
  elem <- grepl(pattern, nlst)
  stopifnot(sum(elem)>0)
  lst  <- lst[elem]
  nlst <- names(lst)
  slst <- strsplit(nlst, '.', fixed=TRUE)
  if (!is.null(name)) {
    ret  <- '<!DOCTYPE html><html><head>'
    if (mathjax) {
      ret  <- c(ret, '<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>')
    }
    ret <- c(ret, '</head><body>')
    if (header>0) lastheader <- rep('', header)
    for (i in 1:length(lst)) {
      if (header>0) {
        if (any(lastheader!=slst[[i]][1:header])) {
          if (i>1) ret <- c(ret, "</table>")
          lastheader <- slst[[i]][1:header] 
          ret <- c(ret, sprintf('<h2>%s</h2>', paste(lastheader, collapse=" ")))    
          ret <- c(ret, '<table width="100%">')
        }
      }
      ret <- c(ret, sprintf('<tr><td><div id="%s" width="100%%">%s</div></td>', nlst[i], lst[i]))
      if (header>0) slst[[i]] <- slst[[i]][-(1:header)]
      ret <- c(ret, sprintf('<td style="text-align:right;background-color: grey;">%s</td></tr>', 
                            paste0(slst[[i]], collapse=" "))
      )
    }
    ret <- c(ret, "</table></body></html>")
    if (all(!endsWith(name, c(".html", ".htm")))) name <- paste0(name, '_xml.html')
    writeLines(ret, name)
  }
  invisible(nlst)
}