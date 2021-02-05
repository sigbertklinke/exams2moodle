#' moodle_text
#'
#' Extracts information from the XML list about questions.
#'
#' @param xml list: a list with XML tags and bodys
#' @param newtext list: modified list with questions, feedbacks and further informations (default: \code{NULL}) 
#'
#' @return list with questions, feedbacks and further informations
#' @export
#'
#' @examples
#' x <- 1
moodle_text <- function(xml, newtext=NULL) {
  qind  <- which(is.tag(xml, "question"))
  ret <- vector("list", length(qind))  
  # type
  for (i in seq(qind)) ret[[i]] <- list(type=xml[[qind[i]]]['type'])
  # name
  ind <- which(is.tag(xml, "name"))
  bin  <-  as.integer(cut(ind, c(qind, length(xml))))
  if (length(bin)) {
    for (i in 1:length(bin)) {
      ret[[bin[i]]]$name <- xml[[ind[i]+2]]
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
        ret[[bin[i]]][[tag]] <- tags[[tag]](xml[[ind[i]+1]])
        attr(ret[[bin[i]]][[tag]], "names") <- names(xml[ind[i]+1])
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
        ret[[i]][[tag]] <- unlist(xml[ind[indu]+1])
        attr(ret[[i]][[tag]], "names")   <- names(xml[ind[indu]+1])
        ret[[i]][[paste0(tag, '_attr')]] <- do.call(rbind, xml[ind[indu]])
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
        ret[[i]][[tag]] <- uncdata(unlist(xml[ind[indu]+2]))
        attr(ret[[i]][[tag]], "names")   <- names(xml[ind[indu]+2])
        ret[[i]][[paste0(tag, '_attr')]] <- do.call(rbind, xml[ind[indu]])
      }
    }
  }
  #browser()
  if (is.null(newtext)) return(ret)
  stopifnot(length(ret)==length(newtext))
  stopifnot(sapply(ret, length)==sapply(newtext, length))
  for (i in 1:length(ret)) {
    for (name in names(ret[[i]])) {
      if (any(ret[[i]][[name]]!=newtext[[i]][[name]])) {
        pos <- names(ret[[i]][[name]])
        if (!is.null(pos)) {
          for (k in 1:length(ret[[i]][[name]])) {
            if (name %in% c("penalty", "defaultgrade", "tolerance", "answernumbering")) {
              xml[[pos[k]]] <- as.character(newtext[[i]][[name]][k])
            }
            if (name %in% c("shuffleanswers", "single")) {
              xml[[pos[k]]] <- if (newtext[[i]][[name]][k]) "true" else "false"
            }
            if (name %in% c("file")) {
              xml[[pos[k]]] <- newtext[[i]][[name]][k]
            }
            if (name %in% c("questiontext", "generalfeedback", "answer")) {
              xml[[pos[k]]] <- cdata(newtext[[i]][[name]][k]) 
            }
          }
        }
      }
    }
  }
  xml
}
#xml  <- moodle_xml2list("/home/sk/syncthing/Seafile/Meine Bibliothek/Statistik Klausur/moodle/01-grund.xml")
#html <- moodle_text(xml)