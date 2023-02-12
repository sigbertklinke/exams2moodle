#' template
#'
#' A text template where R code can be embedded.
#'
#' @param tmpl character: template
#' @param ... named parameter used in the template
#'
#' @return character where the R code is replaced by its evaluation
#' @importFrom stringr str_extract_all
#' @export
#'
#' @examples
#' tmpl <- "`r a`+`r b`"
#' template(tmpl, a=1, b=2)
template <- function(tmpl, ...) {
  oo <- options("scipen"=getOption('exams.scipen', 25))
  on.exit(options(oo))
  e    <- list2env(list(...))
  rtxt <- unique(str_extract_all(tmpl, "`r .*?`")[[1]])
  rcode <- substring(rtxt, 4, nchar(rtxt)-1)
  rres  <- rep(NA_character_, length(rcode))
  for (i in seq_along(rcode)) {
    reval <- as.character(eval(parse(text=rcode[i]), envir=e))
    tmpl  <- gsub(rtxt[i], reval, tmpl, fixed=TRUE)
  }
  tmpl
}