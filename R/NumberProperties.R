#' @rdname NumberProperties
#' @aliases divisor_25 is_terminal prime_numbers primes is_integer, all_integer
#' @title divisor_25, is_terminal, has_digits, all_integer, denominator_25, round_25
#' @description 
#' * `is_terminal` checks whether `x`'s can be expressed as a terminal fraction, basically `divisor_25(denominator(x)))`
#' * `divisor_25` checks all `x`'s can be expressed as \eqn{2^x 5^y}
#' * `prime_numbers` returns all prime numbers up to a limit
#' * `primes` prime factorization of `x`, returns a matrix with the power for each prime number
#' * `has_digits` checks whether the `x`'s have only \code{digits} after the decimal point, basically `abs(x-round(x, digits))<tol`
#' * `all_integer` checks whether all `x`'s are integer, basically `all(has_digits(x,0))`
#' 
#' @param x numeric: values to test/check
#' @param n integer: find all prime numbers up to n
#' @param sieve logical: should in any case the Sieve of Eratosthenes be used to compute prime numbers (default: \code{FALSE})
#' @param min integer: minimal prime number used (default: \code{2})
#' @param digits numeric: number of digits to check (default: \code{2})
#' @param tol numeric: max. deviation from the rounded `x` (default: \code{1e-6})
#' 
#' @md
#' @return logical
#' @export
#'
#' @examples
#' is_terminal(2/3)   # 0.6666... non-terminal
#' is_terminal(1/5)   # 0.2       terminal
#' divisor_25(1:25)
#' prime_numbers(100) # all prime numbers less equal 100
#' primes(1:20)       # prime factorization of 1 to twenty
divisor_25 <- function(x) {
  x   <- as.vector(x)
  repeat {
    by2 <- (x%%2)==0
    by5 <- (x%%5)==0
    x[by2] <- x[by2]%/%2
    x[by5] <- x[by5]%/%5
    if (all(!by2, !by5)) break
  }
  x==1  
}

#' @rdname NumberProperties
#' @export
denominator_25 <- function(x) {
  fracs <- if ('fractions' %in% class(x)) x else fractions(x)
  txt   <- strsplit(attr(fracs, "fracs"), "/", fixed=TRUE)
  denom <- sapply(txt, function(e) { 
    r <- 1
    if (length(e)>1) { 
      r <- as.integer(e[2]) 
      if (is.na(r)) {
        r <- as.numeric(e[2])
        if (is.na(r)) stop(sprintf("can not convert '%s' to integer or numeric", e[2]))
      }
    }
    r
  })
  twofive <- matrix(0, nrow=length(denom), ncol=3, dimnames = list(1:length(x), c("by2", "by5", "rem")))  
  repeat {
    by2 <- (denom%%2)==0
    by5 <- (denom%%5)==0
    twofive[,1:2] <- twofive[,1:2]+cbind(by2, by5)
    denom[by2] <- denom [by2]%/%2
    denom[by5] <- denom [by5]%/%5
    if (all(!by2, !by5)) break
  }
  twofive[,3] <- denom
  round(twofive, 0)
}

#' @rdname NumberProperties
#' @export
is_terminal <- function(x) {
  fracs <- if ('fractions' %in% class(x)) x else fractions(x)
  txt   <- strsplit(attr(fracs, "fracs"), "/", fixed=TRUE)
  denom <- sapply(txt, function(e) { 
    r <- 1
    if (length(e)>1) { 
      r <- as.integer(e[2]) 
      if (is.na(r)) {
        r <- as.numeric(e[2])
        if (is.na(r)) stop(sprintf("can not convert '%s' to integer or numeric", e[2]))
      }
    }
    r
  })
  divisor_25(denom)
  #    sdenom <- sum(denom)
  #    denom  <- ifelse(denom%%2==0, denom/2, denom)
  #    denom  <- ifelse(denom%%5==0, denom/5, denom)
  #    if (sum(denom)==sdenom) return(all(denom==1))
  #  }
  #  return(TRUE) 
}

#' @rdname NumberProperties
#' @export
round_25 <- function(x) {
  d25 <- denominator_25(x)
  dig <- max(ceiling(log10(max(2^d25[,1]*5^d25[,2]))))
  ret <- rep(NA, length(x))
  ret[d25[,3]==1] <- round(x[d25[,3]==1], dig)
  ret
}

#' @rdname NumberProperties
#' @export
prime_numbers <- function(n, sieve=FALSE) {
  base <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 
            101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 
            197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 
            311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 
            431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 
            557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 
            661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 
            809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 
            937, 941, 947, 953, 967, 971, 977, 983, 991, 997)
  if ((n<=1000) && !sieve) return(base[base<=n])
  # Sieve of Eratosthenes
  base <- c(FALSE, rep(TRUE, n-1))
  for (i in 2:floor(sqrt(n))) {
    if (base[i]) base[i*(2:(n%/%i))] <- FALSE
  }
  which(base)
}

#' @rdname NumberProperties
#' @export
primes <- function(x, min=2) {
  prim_num <- prime_numbers(max(min, ceiling(max(x)/2)))
  ret      <- matrix(0, nrow=length(x), ncol=length(prim_num))
  colnames(ret) <- prim_num
  k        <- 1
  while(k<=length(prim_num)) {
    m  <- x%%prim_num[k]
    m0 <- which(m==0)
    if (length(m0)) {
      x[m0]   <- x[m0]%/%prim_num[k]
      ret[m0,k] <- ret[m0,k]+1
    } else {
      k <- k+1
    }
  }
  return(ret)
}

#' @rdname NumberProperties
#' @export
has_digits <- function(x, digits=2, tol=10^{-digits-6}) {
  abs(x-round(x, digits))<tol
}

#' @rdname NumberProperties
#' @export
all_integer <- function(x) {
  isTRUE(all(abs(x-round(x, 0))<1e-9))
}