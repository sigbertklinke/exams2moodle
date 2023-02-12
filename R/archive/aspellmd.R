#' aspellmd
#'
#' Runs a `aspell` on RMarkdown files and excludes some words. The result will be printed and 
#' the result of `aspell` returned invisibly.
#'
#' @inheritParams utils::aspell
#' @param exclude character: words to exclude from checking
#' @param silent logical: indicates if output should be written to the screen (default: `FALSE`)
#'
#' @return invisibly the "cleaned" `aspell` object
#' @importFrom utils aspell
#' @export
#'
#' @examples
#' # none
aspellmd <- function(files,  control = list(), encoding = "unknown",
                     program = NULL, dictionaries = character(), 
                     exclude= c("Meta", "information", "extype", "num",
                                "exsolution", "extol", "exname", "Question"),
                     silent=FALSE) {
  as  <- utils::aspell(files, 'md', control, encoding, program, dictionaries)
  ind <- which(as$Original %in% exclude)
  as  <- as[-ind,]
  as  <- as[order(as$File, as$Line, as$Column),]
  if (!silent && (nrow(as)>0)) {
    lf  <- ''
    for (i in 1:nrow(as)) {
      if (lf!=as$File[i]) {
        lf <- as$File[i]
        cat (lf, "\n")
      }
      cat(sprintf("%4i(%4i): %s -> %s", as$Line[i], as$Column[i], as$Original[i],
                  paste0(as$Suggestions[[i]], collapse=", ")), "\n")
    }
  }
  invisible(as)
}