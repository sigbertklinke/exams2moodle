#' data_prob2
#'
#' Generates a `nrow`x`ncol` matrix with probabilities/frequencies. 
#' If `data` is given it will be normalized such that `sum(data[is.finite(data)])==1`.
#' If no `rownames` or `colnames` are given then event names from `LETTERS` are used. 
#' The returned matrix will have the following attributes:
#' * `marginals` a list of the row anc column marginal distributions,
#' * `byrow` a matrix with conditional probabilities by row,
#' * `bycol` a matrix with conditional probabilities by column,
#' * `expected` a matrix with the expected probabilities under independence, and
#' * `prob` a vector of all probabilities computed (except the expected ones).
#'
#' @param data an optional data vector. Non-atomic classed R objects are coerced 
#' by `as.vector` and all attributes discarded.
#' @param nrow numeric: desired number of rows (default: `2`)
#' @param ncol numeric: desired number of columns (default: `2`)
#' @param colnames character: names of column events
#' @param rownames character: names of row events
#' @param ... further parameters given to [exams2moodle::ddiscrete()]
#'
#' @return a matrix and some attributes
#' @export
#'
#' @examples
#' x <- data_prob2()
#' str(x)
#' data_prob2(colnames="E")
#' data_prob2(nrow=3)
data_prob2 <- function(data=NULL, nrow=2, ncol=2, colnames=NULL, rownames=NULL, ...) {
  not <- function(ev) {  gsub("!!", "", paste0("!", ev), fixed=TRUE) }
  #
  stopifnot((nrow>1) && (ncol>1))
  #
  if (is.null(data)) data <- ddiscrete(runif(nrow*ncol), ...)
  data <- data/sum(data[is.finite(data)])
  events <- LETTERS
  nfill  <- nrow-length(rownames)
  if (nfill>0) {
    if (nrow>2) {
      rownames <-  c(rownames, events[1:nfill])
    } else {
      if (nfill==2) rownames <- c(events[1], not(events[1]))
      if (nfill==1) rownames <- c(rownames, not(rownames))
    }
  } 
  if (nfill<0) rownames <- rownames[1:nrow]
  #
  events <- setdiff(events, rownames)
  nfill  <- ncol-length(colnames)
  if (nfill>0) {
    if (ncol>2) {
      colnames <-  c(colnames, events[1:nfill])
    } else {
      if (nfill==2) colnames <- c(events[1], not(events[1]))
      if (nfill==1) colnames <- c(colnames, not(colnames))
    }
  } 
  if (nfill<0) colnames <- colnames[1:ncol]
  #
  ret <- matrix(data, ncol=ncol, nrow=nrow, dimnames=list(rownames,colnames))
  #
  prob <- numeric(0)
  ev   <- outer(rownames(ret), colnames(ret), function(row, col) { paste0(row, "^", col) })
  for (i in 1:length(ret)) prob[ev[i]] <- ret[i]
  rret <- rowSums(ret)
  for (i in names(rret)) prob[i] <- rret[i]
  cret <- colSums(ret)
  for (i in names(cret)) prob[i] <- cret[i]
  ev    <- outer(rownames(ret), colnames(ret), function(row, col) { paste0(row, "|", col) })
  brret <- proportions(ret, 1)
  for (i in 1:length(brret)) prob[ev[i]] <- brret[i]
  ev    <- outer(rownames(ret), colnames(ret), function(row, col) { paste0(col, "|", row) })
  bcret <- proportions(ret, 2)
  for (i in 1:length(bcret)) prob[ev[i]] <- bcret[i]
  expected <- structure(rret%o%cret, dimnames=dimnames(ret))
  structure(ret, marginals=list(rret, cret), byrow=brret, bycol=bcret, 
            expected=expected, prob=prob)
}