#' warningc
#'
#' Compactified warning messages.
#'
#' @param ... further parameters (unused) 
#'
#' @return nothing
#' @export 
#'
#' @examples
#' test <- function() {
#'   warning("Warning 1")
#'   warning("Warning 1")
#'   warning("Warning 2")
#' }
#' test()
#' warningc()
warningc <- function(...) {
  if (length(last.warning <- baseenv()[["last.warning"]])) {
    warn.name <- names(last.warning)
    warn.msg  <- sapply(last.warning, function(e) { e[1] })
    warn      <- paste(warn.name, ':',  warn.msg)
    nr        <- as.character(1:length(warn))
    dup       <- duplicated(warn)
    i         <- length(warn)
    while (i>0) {
      if (dup[i]) {
        pos     <- min(which(warn==warn[i]))
        nr[pos] <- paste0(nr[pos], ',', i)
        nr[i]   <- ''
      }
      i <- i-1
    }
    for (i in 1:length(warn)) {
      if (!dup[i]) cat("\n", nr[i], ":\n  ", warn.name[i], "\n  ", warn.msg[i])
    }
  }
}