#' @rdname Combinatorics
#' @title Combinatorics
#'
#' * `permutation` computes the number of permutations
#' * `variation` computes the number of variations with and without replication  
#' * `combination` computes the number of combinatios with and without replication  
#' * `combinatorics` computes all combinatorics results for k<n and returns it as list:
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
#' @param n numeric: total number of elements
#' @param k numeric: number of elements to choose 
#' @param repl logical: with reptition (default: \code{FALSE})
#'
#' @return a list
#' @export
#'
#' @examples
#' permutation(8)
#' permutation(8, c(1,3,2,2))
#' combination(8, 4)
#' combination(8, 4, TRUE)
#' variation(8, 4)
#' variation(8, 4, TRUE)
#' combinatorics(8, 4)
combinatorics <- function(n, k) {
  ret <- list(permutation.n=permutation(n),
              permutation.k=permutation(k),
              permutation.nk=permutation(n, k),
              variation = variation(n, k),
              variation.rep = variation(n, k, TRUE),
              combination = combination(n, k),
              combination.rep = combination(n, k, TRUE)
             )
  attr(ret, "mindiff") <- min(diff(sort(unlist(ret))))
  ret
}

#' @rdname Combinatorics
#' @export
variation   <- function(n, k, repl=FALSE) { if (repl) n^k else factorial(n)/factorial(n-k) }

#' @rdname Combinatorics
#' @export
combination <- function(n, k, repl=FALSE) { if (repl) choose(n+k-1, k) else choose(n, k) }

#' @rdname Combinatorics
#' @export
permutation <- function(n, k=rep(1, n)) { factorial(n)/prod(factorial(k)) }