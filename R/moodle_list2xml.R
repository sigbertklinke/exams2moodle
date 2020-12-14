#' moodle_list2xml
#'
#' Writes a XML list, read with \code{moodle_xml2list}, to a file.
#'
#' @param taglist list: list of XML tags and bodies
#' @param file character: XML file name
#' @param quiet logical: generate output during writing (default: \code{FALSE})
#'
#' @return nothing
#' @importFrom stringr str_split str_sub str_locate_all
#' @importFrom utils tail head
#' @export
#'
#' @examples
#' file <- system.file('xml', 'rexams.xml', package="exams2moodle")
#' xml  <- moodle_xml2list(file, FALSE)
#' tmpfile <- tempfile()
#' moodle_list2xml(xml, tmpfile)
#' # now compare both files, except for some empty lines there should be no difference  
moodle_list2xml <- function(taglist, file, quiet=FALSE) {
  maketags <- function(tag) {
    c(paste0(
      "<",
      paste0(
        names(tag),
        ifelse((nchar(names(tag))>0) & (!is.na(tag)), '=', ''),
        ifelse(is.na(tag), '', tag),
        collapse=" "
      ),
      ">"),
      paste0("</", tag[1], ">")
    )
  }
  # xml
  keys   <- lapply(str_split(names(taglist), ','), as.integer)
  tags   <- maketags(taglist[[1]]);  
  txt    <- paste0(tags[1], "\n")
  endtag <- ''
  for (i in 2:length(keys)) {
    # close tags
    while (length(endtag)>length(keys[[i]])) {
      txt <- c(txt,  endtag[1])
      endtag <- endtag[-1]
    }
    if (length(endtag)==length(keys[[i]])) {
      le <- length(endtag)
      if (keys[[i-1]][le]!=keys[[i]][le]) {
        txt <- c(txt, endtag[1])
        endtag <- endtag[-1]       
      }
    } 
    # new tag
    if (tail(keys[[i]], 1)==0) { # block 
      txt <- c(txt, taglist[[i]])
    } else { # no block
      tags <- maketags(taglist[[i]]);   
      if (!quiet) cat(sprintf("%*s\n", 2*length(keys[[i]])+nchar(tags[1])-4, tags[1]))
      nolinebreak <- (i<length(keys)) && (tail(keys[[i+1]], 1)==0)
      txt <- c(txt, paste0(tags[1], ifelse(nolinebreak, '', "\n")))
      endtag <- c(paste0(tags[2], ifelse(tags[2]=='</question>', "\n\n\n", "\n")), endtag)
    }
  }
  # close all open tags
  if (length(endtag)) txt <- c(txt, endtag)
  writeLines(paste0(txt, collapse=""), file)
}