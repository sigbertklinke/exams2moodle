#' grade
#'
#' Computes a grade based on points to grade scheme by the Humboldt-Universität zu Berlin, see 
#' §96c and §102 in the [Achte Änderung der Fächerübergreifenden Satzung zur Regelung von Zulassung, Studium und Prüfung der Humboldt-Universität zu Berlin (ZSP-HU)](https://gremien.hu-berlin.de/de/amb/2020/11/11_2020_zsp-hu_2013_ae08-2020_2020-05-29_druck.pdf#page=5)
#'
#' @param points numeric: points achieved in exam
#' @param maxpts numeric: maximal number of achievable points in exam (default: \code{max(points)})
#' @param fixed logical: fixed or relative grade scheme (default: \code{TRUE})
#'
#' @return grades as function of points
#' @export
#'
#' @examples
#' x <- round(runif(100, 0, 22.4))
#' grade(x, 22)
grade <- function(points, maxpts=max(points), fixed=TRUE) {
  pts <- as.numeric(points)
  minpts <- if (fixed) 0.5*maxpts else max(0.4*maxpts, 0.9*mean(pts))
  rempts <- maxpts-minpts
  # 1 = 1.0, 1.3, 2 = 1.7, 2.0, 2.3, 3 = 2.7, 3.0, 3.3, 4 = 4.0, 3.7  
  gpts   <- c(0, minpts+rempts*c(0, 0.125, 
                                 0.25, 0.25+0.25/3, 0.25+2*0.25/3,
                                 0.5,  0.5+0.25/3,  0.5+2*0.25/3, 
                                 0.75, 0.875, 1))
  levels <- c("5.0", "4.0", "3.7", "3.3", "3.0", "2.7", "2.3", "2.0", "1.7", "1.3", "1.0")
  grdtxt <- ordered(levels[cut(pts, gpts, labels=FALSE, include.lowest=TRUE)],
                    levels=levels)
  attr(grdtxt, "gradebreaks") <- gpts
  grdtxt
}