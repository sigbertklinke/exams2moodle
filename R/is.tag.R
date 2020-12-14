#' is.tag
#'
#' Returns a logical vector if the entry in \code{xml} is a tag or body. If \code{tagname} is given then 
#' it returns \code{TRUE} if the tag name appears in \code{tagname}.
#'
#' @param xml list: xml list with tags and bodies
#' @param tagname character: vector of tag names (default: \code{NULL})
#'
#' @return a logical vector
#' @importFrom stringr str_split
#' @importFrom utils tail
#' @export
#'
#' @examples
#' file <- system.file('xml', 'rexams.xml', package="exams2moodle")
#' xml  <- moodle_xml2list(file, FALSE)
#' # position body entries 
#' which(!is.tag(xml))
#' # position of question tags
#' which(is.tag(xml, "question"))
is.tag <- function(xml, tagname=NULL) {
  # xml
  keys <- lapply(str_split(names(xml), ','), as.integer)
  res  <- sapply(keys, function(e) { tail(e,1)>0 })
  if (!is.null(tagname)) {
    res <- res & sapply(xml, function(e) { e[1] %in% tagname })
  }
  res
}
