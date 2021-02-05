.onLoad <- function(libname, pkgname){
  utils::data("distributions", package=pkgname, envir=parent.env(environment()))
}