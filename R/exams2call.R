#' exams2call
#'
#' Returns a list with the function name and parameters called from the \code{.traceback()}. 
#' The function name must start with \code{"exams2"}
#'
#' @param prefix character: start of the function name (default: \code{"exams2"})
#'
#' @return a list with the function name and its evaluated parameters
#' @export
#'
#' @examples
#' exams2call()                 # access current call stack
exams2call <- function(prefix="exams2") {
  calls <- sys.calls()
  if (length(calls)==0) return (NULL)
  for (i in 1: length(calls)) {
    cmd <- as.character(calls[[i]][[1]])
    if ((length(cmd)==1) && startsWith(cmd, prefix)) {
      ret <- as.list(match.call(match.fun(cmd), calls[[i]]))
      ret[[1]] <- cmd
      if (length(names(ret))) {
        for (name in names(ret)) {
           if (is.symbol(ret[[name]]) || is.language(ret[[name]])) ret[[name]] <- eval(ret[[name]], envir=i)
        }
      }
      return(ret)
    }
  }
  return(NULL)
}