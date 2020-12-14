#' moodle_fraction
#' 
#' Extracts and sets from the XML list all fraction attributes from question tags.
#' If \code{newfraction} is \code{NULL} then list all fraction attributes
#' is created and returned separately for each question. Otherwise the entries
#' in \code{newfraction} replace the ones in the XML list.
#'
#' @param xml list: list of XML tags and bodies
#' @param newfraction list: fraction values (default: \code{NULL})
#'
#' @return If \code{newfraction} is \code{NULL} then it returns a list with fractions.
#'         Otherwise it returns the modified XML list.
#' @importFrom stringr str_sub
#' @export
#'
#' @examples
#' file <- system.file('xml', 'rexams.xml', package="exams2moodle")
#' xml  <- moodle_xml2list(file, FALSE)
#' fraction <- moodle_fraction(xml)
#' fraction[[2]]
#' fraction[[2]] <- c(0,0,100,100,0)
#' xml <- moodle_fraction(xml, fraction)
moodle_fraction <- function(xml, newfraction=NULL) {
  #rowser()
  qind <- which(is.tag(xml, "question"))
  aind <- which(is.tag(xml, "answer"))
  binn <- as.integer(cut(aind, c(qind, length(xml))))
  ret  <- vector("list", length(qind)) 
  for (i in 1:length(qind)) {
    ret[[i]] <- sapply(xml[aind[binn==i]], function(e) { 
      frac <- e['fraction'] 
      as.numeric(str_sub(frac, 2, nchar(frac)-1))
    })
  }
  if (is.null(newfraction)) return(ret)
  stopifnot(length(ret)==length(newfraction))
  stopifnot(sapply(ret, length)==sapply(newfraction, length))
  validfraction <- moodle_fractions()
  for (i in seq(ret)) {
    if (length(ret[[i]])) {
      rnf <- round(newfraction[[i]],5)
      stopifnot(all(rnf %in% validfraction))
      keys <- names(ret[[i]])
      for (j in seq(keys)) {
        xml[[keys[j]]]['fraction'] <- sprintf("\"%s\"", toString(rnf[j]))
      }
    }
  }
  xml
}