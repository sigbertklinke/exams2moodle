#' ttests
#'
#' `ttests` runs a bunch of modifications of the input parameters of `ttest` to generate all possible t-tests. 
#' See under Details the detailed parameter values which are used. Note that not giving the parameter `hyperloop` will
#' results in approx. 5000 t-tests generated. 
#' Returned will be only the different t-tests with the first element is `ttest`. If only a specific element of a `ttest` 
#' is of interest then just give the name of the element in `elem` and then all `ttest`s will be returned where `elem` is different.
#' 
#' @param ttest ttest: the base result from a valid t-test generated by [ttest_num()]
#' @param elem character: element to extract (default: `NULL`)
#' @param hyperloop named list: parameter values to run over (default: see above)
#'
#' @details The default `hyperloop` is
#' ```
#' list(n           = c(1, ttest$n, ttest$n+1),
#'      mu0         = c(ttest$mu0, ttest$mean),
#'      mean        = c(ttest$mu0, ttest$mean), 
#'      sigma       = c(ttest$sigma, ttest$sd, sqrt(ttest$sigma), sqrt(ttest$sd)),
#'      sd          = c(ttest$sigma, ttest$sd, sqrt(ttest$sigma), sqrt(ttest$sd)),
#'      norm        = c(TRUE, FALSE),
#'      alpha       = unique(c(ttest$alpha, 0.01, 0.05, 0.1)),
#'      alternative = c("two.sided", "greater", "less")
#'     )
#' ````
#'
#' @return list of `ttest` objects is returned 
#' @export
#' @md
#'
#' @examples
#' basetest  <- ttest_num(mean=0.5, sd=1.25, n=50, sigma=1)
#' # vary the number of observations
#' hyperloop <- list(n=c(1, basetest$n, basetest$n^2))
#' # return all different t-tests
#' tts       <- ttests(basetest, hyperloop=hyperloop)
#' # return all different random sampling functions
#' ttests(basetest, "Xbar", hyperloop)
ttests <- function(ttest, elem=NULL, hyperloop=NULL) {
  if (is.null(hyperloop)) {
    hyperloop  <- list(n           = c(1, ttest$n, ttest$n+1),
                       mu0         = c(ttest$mu0, ttest$mean),
                       mean        = c(ttest$mu0, ttest$mean), 
                       sigma       = c(ttest$sigma, ttest$sd, sqrt(ttest$sigma), sqrt(ttest$sd)),
                       sd          = c(ttest$sigma, ttest$sd, sqrt(ttest$sigma), sqrt(ttest$sd)),
                       norm        = c(TRUE, FALSE),
                       alpha       = unique(c(ttest$alpha, 0.01, 0.05, 0.1)),
                       alternative = c("two.sided", "greater", "less")
    )
  }
  hyperindex <- lapply(hyperloop, function(e){seq(e)} )
  loop       <- expand.grid(hyperindex)
  #1
  ret        <- vector("list", 1+nrow(loop))
  ret[[1]]   <- ttest
  keep       <- rep(NA_character_, 1+nrow(loop))
  keep[[1]]  <- toString(serialize(ttest, NULL))
  # all others
  loopname   <- names(hyperloop)
  for (i in 1:nrow(loop)) {
    ltest <- ttest  
    for (name in loopname) {
      j             <- loop[[name]][i]
      ltest[[name]] <- hyperloop[[name]][j]
    }
    ltest       <- try(ttest_num(arglist=ltest), silent=TRUE)
    if ("try-error" %in% class(ltest)) {
      ret[[i+1]]  <- ret[[1]]
      keep[[i+1]] <- toString(serialize(ret[[1]], NULL))      
    } else {
      ret[[i+1]]  <- ltest 
      keep[[i+1]] <- toString(serialize(ltest, NULL))      
    }
  } 
  # clean up 
  keep <- !duplicated(keep) 
  ret  <- ret[keep]
  if (is.null(elem)) return(ret)
  keep <- sapply(ret, function(e) { toString(serialize(e[[elem]], NULL))})   
  keep <- !duplicated(keep)
  ret[keep]
}