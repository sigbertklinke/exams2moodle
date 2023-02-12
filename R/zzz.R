#' @import extraDistr rjson tinytex yaml

pkgenv <- new.env()

.onLoad <- function(libname, pkgname){
  pkgenv$opening <- c("("="\\left(", "["="\\left[", "{"="\\left\\{", "|"="|", "||"="\\|", 
                      "<"="\\langle", "a"="\\langle", "c"="\\lceil", "f"="\\lfloor")
  pkgenv$closing <- c("("="\\right)", "["="\\right]", "{"="\\right\\}{", "|"="|", "||"="\\|", 
                      "<"="\\rangle", "a"="\\rangle", "c"="\\rceil", "f"="\\rfloor")
  utils::data("distributions", package=pkgname, envir=parent.env(environment()))
}