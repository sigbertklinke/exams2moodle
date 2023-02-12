#' spell
#'
#' Perform a spell check on RMarkdown files ignoring some `exams` keywords using [spelling::spell_check_files()].
#' 
#' @inheritParams spelling::spell_check_files
#'
#' @return a data frame with problematic words
#' @importFrom spelling spell_check_files
#' @export
#'
#' @examples
#' # none
spell <- function(path, ignore=c(
  "Meta", "information", "extype", "num", "mchoice", "schoice",
  "Solution", "exsolution", "extol", "exname", "Question",         # keywords exams
  "align", "begin", "bigg", "cases", "cdot", "end", "frac", 
  "infty", "int", "left", "left.", "leq", "mu", "qquad", "right",
  "sum", "text", "vert"                                            # keywords LaTeX
  ),
                  lang = Sys.getenv("LANG")) {
  lang <- strsplit(lang, "[^A-Za-z]")[[1]]
  if (length(lang)>1) lang <- paste0(lang[1:2], collapse='_')
  spelling::spell_check_files(path, ignore, lang)
}