#' moodle_fraction
#'
#' Returns a vector with all possible Moodle fractions, see also \code{exams:::moodleFractions}
#'
#' @param all logical: return all fractions or only positive and zero fractions (default: \code{TRUE})
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' moodle_fractions()
#' moodle_fractions(FALSE)
moodle_fractions <- function(all=TRUE) {
  mf <- unique(sort(100*c(seq(0,1,1/4), seq(0,1,1/6), seq(0,1,1/10), 1/c(7,8,9,20)), decreasing=TRUE))
  if (all) mf <- unique(sort(c(mf, -mf), decreasing=TRUE))
  round(mf, 5)
}
