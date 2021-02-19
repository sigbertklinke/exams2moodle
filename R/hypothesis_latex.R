#' hypothesis_latex
#'
#' Creates a data frame with two columns \code{H0} and \code{H1} containing latex representations of the null and the alternative 
#' hypothesis of a test. If \code{null} is not given then it determined from \code{alternative}. The terms 
#' \code{"two.sided"},  \code{"greater"}, and  \code{"less"} for \code{alternative} and \code{null} are interpreted 
#' as in \code{t.test}. 
#' 
#' The terms \code{"lt"},  \code{"le"}, \code{"eq"},  \code{"ne"}, \code{"ge"}, and \code{"gt"} are interpreted as is. 
#' Note that abbreviations, for example \code{alternative="t"} for a two sided test, can not be used.
#' 
#' @param left character: symbol, for example \code{"\\mu"} or \code{"\\pi"}
#' @param alternative character: alternative hypotheses
#' @param null character: null hypothese (default: \code{NULL})
#' @param right character: symbol (default: \code{paste0(left, "_0")})
#'
#' @return data frame with hypothesis pairs
#' @export
#'
#' @examples
#' hypothesis_latex("\\mu")
#' hypothesis_latex("\\pi")
#' hypothesis_latex("\\mu", alternative="two.sided")
#' hypothesis_latex("\\mu", alternative="two.sided", null="lt")
#' hypothesis_latex("\\mu", alternative="ne", null="eq")
hypothesis_latex <- function (left, alternative=c("two.sided", "less", "greater"), null=NULL,
                              right=paste0(left, "_0")) {
  hyp <- list()
  hyp[["two.sided"]] <- c("=", "\\neq")
  hyp[["less"]]      <- c("\\geq", "<")
  hyp[["greater"]]   <- c("\\leq", ">")
  hyp[["ne"]] <- c("\\neq", "\\neq") 
  hyp[["gt"]] <- c(">", ">") 
  hyp[["lt"]] <- c("<", "<") 
  hyp[["le"]] <- c("\\leq", "\\leq")
  hyp[["ge"]] <- c("\\geq", "\\geq")
  hyp[["eq"]] <- c("=", "=")
  h0 <- sapply(hyp, "[", 1)
  h1 <- sapply(hyp, "[", 2)
  if (is.null(null)) null <- alternative
  h0 <- h0[null]
  h1 <- h1[alternative]
  stopifnot(length(h0)==length(h1))
  less      <- (h0=="\\geq") & (h1=="<")
  greater   <- (h0=="\\leq") & (h1==">")
  two.sided <- (h0=="=") & (h1=="\\neq")
  ret <- cbind(paste(left, h0, right), paste(left, h1, right))
  colnames(ret)  <- c("H0", "H1")
  row.names(ret) <- sprintf("wrong%0.f", 1:nrow(ret))
  if (any(less)) row.names(ret)[which(less)] <- "less"
  if (any(greater)) row.names(ret)[which(greater)] <- "greater"  
  if (any(two.sided)) row.names(ret)[which(two.sided)] <- "two.sided"    
  if (anyDuplicated(ret)) warning ("duplicate hypothesis pairs")
  as.data.frame(ret)
}