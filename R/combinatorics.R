#' combinatorics 
#'
#' Computes all combinatorics results for k<n and returns it as list:
#' \describe{
#' \itemize{\code{permutation.n}}{\eqn{P(n)}}
#' \itemize{\code{permutation.k}}{\eqn{P(k)}}
#' \itemize{\code{permutation.nk}}{\eqn{P(n; k)}}
#' \itemize{\code{variation}}{\eqn{V(n;k)}}
#' \itemize{\code{variation.rep}}{\eqn{V^W(n;k)}}
#' \itemize{\code{combination}}{\eqn{K(n;k)}}
#' \itemize{\code{combination.rep}}{\eqn{K^W(n;k)}}
#' }
#'
#' @param n total number of elements
#' @param k number of elements to choose
#'
#' @return a list
#' @export
#'
#' @examples
#' combinatorics(4,2)
combinatorics <- function(n, k) {
  ret <- list(permutation.n=factorial(n),
              permutation.k=factorial(k),
              permutation.nk=factorial(n)/factorial(k),
              variation = factorial(n)/factorial(n-k),
              variation.rep = n^k,
              combination = factorial(n)/(factorial(k)*factorial(n-k)),
              combination.rep = factorial(n+k+1)/(factorial(k)*factorial(n+1))
             )
  attr(ret, "mindiff") <- min(diff(sort(unlist(ret))))
  ret
}