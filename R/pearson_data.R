#' pearson_data
#'
#' Generates an integer data set for computing a correlation using [exams2moodle::sumofsquares()]. 
#' If `n>100` and `nmax>6` it is better to use one of the precomputed solutions. Otherwise it may take
#' up to `maxt` seconds. Note that the correlation of the generated data set may differ from the desired 
#' correlation.
#'
#' @md
#' @param r numeric: desired correlation
#' @param n integer: number to decompose as sum of squares, see [exams2moodle::sumofsquares()]. 
#' @param nmax integer: maximal number of squares in the sum, see [exams2moodle::sumofsquares()]. 
#' @param maxt numeric: maximal number of seconds the routine should run, see [exams2moodle::sumofsquares()]. 
#' @param xsos sos matrix: precomputed matrix
#' @param ysos sos matrix: precomputed matrix
#'
#' @return a matrix with two columns and an attribute `interim` for intermediate values as matrix. 
#' The rows of the matrix contain : \eqn{x_i}, \eqn{y_i},  \eqn{x_i-bar{x}}, \eqn{y_i-\bar{y}}, 
#' \eqn{(x_i-bar{x})^2}, \eqn{(y_i-\bar{y})^2}, and \eqn{(x_i-bar{x})((y_i-\bar{y})}.
#' In a final step a vector with the row sums is appended as further column.
#' @export
#'
#' @examples
#' data(sos)
#' xy <- pearson_data(0.7, xsos=sos100)
#' colSums(xy)
#' colSums(xy^2)
#' sum(xy[,1]*xy[,2])
#' # my data
#' x <- 100+5*xy[,1]
#' y <- 100+5*xy[,2]
#' cor(x, y)
pearson_data <- function(r, n=100, nmax=6, maxt=30, xsos=NULL, ysos=NULL) {
  stopifnot(abs(r)<=1)
  if (is.null(xsos)) {
    xsos <- sumofsquares(n=n, nmax=nmax, maxt=maxt, zerosum = TRUE)
    xsos <- xsos[rowSums(is.na(xsos))==0,1:nmax]    
    ysos <- xsos    
  } else {
    xsos <- xsos[rowSums(!is.na(xsos))==nmax,1:nmax]    
    if (is.null(ysos)) ysos <- xsos
  }
  rxy <- xsos%*%t(ysos)
  rxy <- rxy/n
  rxy <- abs(rxy-abs(r))
  ind <- which(rxy==min(rxy), arr.ind = TRUE)
  if (nrow(ind)>1) ind <- ind[sample(1:nrow(ind), 1),]
  xy  <- cbind(xsos[ind[1],], if(r>0) ysos[ind[2],] else -ysos[ind[2],])
  xyn <- scale(xy, scale=FALSE)    
  m   <- rbind(t(xy), t(xyn), t(xyn^2), xyn[,1]*xyn[,2]) 
  m   <- cbind(m, rowSums(m))
  colnames(m) <- c(1:nmax, "$\\sum$") 
  row.names(m) <- c("$x_i$", "$y_i$", "$(x_i-\\bar{x})$", "$(y_i-\\bar{y})$", 
                    "$(x_i-\\bar{x})^2$", "$(y_i-\\bar{y})^2$", "$(x_i-\\bar{x})(y_i-\\bar{y})$")
  structure(xy, interim=m)
}