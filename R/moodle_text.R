#' moodle_text
#'
#' Extracts information from the XML list about questions.
#'
#' @param xml list: a list with XML tags and bodys
#'
#' @return list with questions, feedbacks and further informations
#' @export
#'
#' @examples
#' x <- 1
moodle_text <- function(xml) {
  qind  <- which(is.tag(xml, "question"))
  html <- vector("list", length(qind))  
  # type
  for (i in seq(qind)) html[[i]] <- list(type=xml[[qind[i]]]['type'])
  # name
  ind <- which(is.tag(xml, "name"))
  bin  <-  as.integer(cut(ind, c(qind, length(xml))))
  if (length(bin)) {
    for (i in 1:length(bin)) {
      html[[bin[i]]]$name <- xml[[ind[i]+2]]
    }
  }
  # penalty, defaultgrade, ...
  tags <- list(penalty=as.integer, defaultgrade=as.integer,
              shuffleanswers=as.logical, single=as.logical,
              answernumbering=as.character, tolerance=as.numeric)
  for (tag in names(tags)) {
    ind <- which(is.tag(xml, tag))
    bin  <-  as.integer(cut(ind, c(qind, length(xml))))
    if (length(bin)) {
      for (i in 1:length(bin)) {
        html[[bin[i]]][[tag]] <- tags[[tag]](xml[[ind[i]+1]])
      }
    } 
  }
  # +1 file
  for (tag in  c("file")) {
    ind <- which(is.tag(xml, tag))
    bin <-  as.integer(cut(ind, c(qind, length(xml))))
    if (length(bin)) {
      binu <- unique(bin)
      for (i in binu) {
        indu <- which(bin==i)
        html[[i]][[tag]] <- unlist(xml[ind[indu]+1])
        html[[i]][[paste0(tag, '_attr')]] <- do.call(rbind, xml[ind[indu]])
      }
    }
  }
  # +2 uncdata: questiontext, generalfeedback, answer
  #browser()
  for (tag in  c("questiontext", "generalfeedback", "answer", "feedback")) {
    ind <- which(is.tag(xml, tag))
    bin <-  as.integer(cut(ind, c(qind, length(xml))))
    if (length(bin)) {
      binu <- unique(bin)
      for (i in binu) {
        indu <- which(bin==i)
        html[[i]][[tag]] <- uncdata(unlist(xml[ind[indu]+2]))
        html[[i]][[paste0(tag, '_attr')]] <- do.call(rbind, xml[ind[indu]])
      }
    }
  }
  # browser()
  html
}
#xml  <- moodle_xml2list("/home/sk/syncthing/Seafile/Meine Bibliothek/Statistik Klausur/moodle/01-grund.xml")
#html <- moodle_text(xml)