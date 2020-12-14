#' to_choice
#'
#' Creates a list with the elements \code{questions} and \code{solutions} values. A value can be either an entry in a vector or
#' a row in a data frame. \code{correct} is logical vectors which contains \code{TRUE} if the values represents a correct answer
#' and \code{FALSE} if it represents a wrong answer. The values can be shuffled or ordered (default).
#' 
#' If \code{shuffle} is a integer of length 1 then one correct answer is choosen and \code{shuffle} wrong answers are choosen.
#' If \code{shuffle} is a integer of length larger than 1 then \code{shuffle[1]} correct answers are choosen and \code{shuffle[2]} wrong answers are choosen.
#' If any \code{shuffle} entry is zero or negative then no shuffling will be done.
#' If \code{order} is a function then it is expected that the function delivers an index for the reordering of the values.
#' Otherwise a shuffle about all values is applied.
#' 
#' The shuffling works in two steps:
#' \enumerate{
#' \item Sample within the correct and wrong value according to \code{shuffle}
#' \item Apply shuffling (\code{order=NULL}) or ordering (default: \code{order=order}) of all selected answers 
#' }
#'
#' @param df vector or data frame: values, in a data frame each row holds one value
#' @param correct logical: answer is correct (\code{TRUE}) or not (\code{FALSE})
#' @param shuffle integer: the numbers of correct and wrong values to shuffle (default: \code{c(NA,NA)}), NA means no shuffling
#' @param orderfun function: ordering of the shuffled values (default: \code{order})
#' @param ... further named parameters used in \code{shuffle}
#'
#' @return list with questions and solutions 
#' @export
#'
#' @examples
#' answer   <- runif(5)
#' correct  <- (1:5)==3 # third answer is correct, the rest wrong
#' sc       <- to_choice(answer, correct)
#' str(sc)           # answers are ordered by size
#' sc$questions <- c(format(sc$questions, nsmall=2), "No answer is correct") # Additional answer
#' sc$solutions <- c(sc$solutions, FALSE)                                    # TRUE or FALSE?
#' sc           <- to_choice(answer, correct, shuffle=2)
#' str(sc)      # one correct answer and two wrong answers selected
to_choice <- function(df, correct, shuffle=c(NA_integer_,NA_integer_), orderfun=order, ...) {
  if (missing(df))      stop ("Parameter 'df' required")
  if (missing(correct)) stop ("Parameter 'correct' required")
  if (!is.data.frame(df)) df <- data.frame(df)
  nc <- sum(correct==TRUE)
  nw <- sum(correct==FALSE)
  stopifnot(nc+nw>0)
  # shuffle
  shuffle <- as.numeric(shuffle)
  if (length(shuffle)==1) shuffle <- c(1, shuffle)
  #
  shufflequestions <- which(correct)
  questions        <- integer(0)
  if (is.na(shuffle[1])) {
    questions <- c(questions, shufflequestions)
  } else {
    if (nc>1) {
      size <- if ((shuffle[1]<1) || (shuffle[1]>nc)) nc else shuffle[1]
      questions <- c(questions, sample(shufflequestions, size))
    } else {
      questions <- c(questions, shufflequestions)
    }
  }
  shufflequestions <- which(!correct)
  if (is.na(shuffle[2])) {
    questions <- c(questions, shufflequestions)
  } else {
    if (nw>0) {
      size <- if ((shuffle[2]<1) || (shuffle[2]>nw)) nw else shuffle[2]
      questions <- c(questions, sample(shufflequestions, size))
    } else {
      questions <- c(questions, shufflequestions)
    }
  }
  if (length(questions)==0) questions <- 1:nrow(df)
  #
  if (!is.null(orderfun)) {
    orderfun <- match.fun(orderfun)   
    args     <- list(...)
    for (i in 1:ncol(df)) {
      args[[1+length(args)]] <- df[questions,i]
    }
    questions <- questions[do.call(orderfun, args)]
  } else {
    questions <- sample(questions, length(questions))
  }
  list(questions = df[questions,], solutions=correct[questions])
}