#' moodle_m2s
#'
#' The \code{rexams} package does not support multiple choice questions with several correct answers but only one allowed answers.
#' The function reads in the Moodle XML file and replaces **all** \code{<single>false</single>} by \code{<single>true</single>}. Also, 
#' **all** fractions are changed, anything below 0 is set to zero and anything above 0 is set to 100. At the end the modified
#' XML code is saved into \code{newfile}. 
#'
#' @param file character: Moodle XML file with exercises to read from
#' @param newfile character:  Moodle XML file to write to (default: \code{file})
#' @param quiet logical: generate output during reading (default: \code{FALSE})
#'
#' @return invisibly the file name written to
#' @export
#' @md
#'
#' @examples
#' x <- 1
moodle_m2s <- function(file, newfile=file, quiet=TRUE) {
  if (!endsWith(file, ".xml")) file <- paste0(file, '.xml')
  xml  <- moodle_xml2list(file, quiet=quiet)
  cont <- moodle_text(xml)
  for (i in 1:length(cont)) {
    if (!is.null(cont[[i]]$single)) cont[[i]]$single <- TRUE
  }
  xml   <- moodle_text(xml, cont)
  frac  <- moodle_fraction(xml)
  nfrac <- lengths(frac)
  for (i in 1:length(cont)) {
    if (nfrac[i]) {
      frac[[i]][frac[[i]]<0] <- 0
      frac[[i]][frac[[i]]>0] <- 100   
    }
  }
  xml <- moodle_fraction(xml, frac)
  if (!endsWith(newfile, ".xml")) newfile <- paste0(newfile, '.xml')
  moodle_list2xml(xml, newfile)
  invisible(newfile)
}