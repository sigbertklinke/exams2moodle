#' @rdname solution
#' @aliases sol_num
#' @aliases sol_int
#' @aliases sol_mc
#' @aliases sol_info
#' @title Solutions
#' @description Creates a `solution` object and prints a meta information block:
#' * `solution` the default is `sol_num`
#' * `sol_num` for a numerical solution
#' * `sol_int` for an integer solution
#' * `sol_mc` for a multiple choice solution
#' * `sol_ans` for the answer list of a multiple choice solution
#' * `sol_tf` for the solution list (True or False) of a multiple choice solution
#' * `sol_info` for creating a Meta-Information block
#' 
#' @param x numeric solution or false MC solutions
#' @param y true MC solutions
#' @param sample integer: sampling numbers for false and/or true solutions (default: `NULL`)
#' @param shuffle logical or function: shuffling or ordering of solutions (default `order`)
#' @param none character: if you do not wish to choose any of the false and true solutions offered (default: `NULL`)
#' @param tol numeric: a tolerance for a numeric solution (default: `NA`)
#' @param digits integer: number of digits for rounding (default: `NA`)
#' @param ... further parameters
#' @details
#' For numerical solutions you can set `tol` and/or `digits`. 
#' If they are not set, they are automatically selected.
#' If `tol` is not set and `length(x)>1` then the tolerance is chosen as `min(diff(sort(x)))/2`.
#' Otherwise as `max(0.001, 0.001*abs(x))`. I
#' If `tol` is negative, `tolerance` is set to `10^tol`, otherwise it is used as it is. 
#' If `digits` is not set, `ceiling(-log10(tolerance))` is used.
#' 
#' @return a `solution` object
#' @export
#'
#' @examples
#' s <- sol_num(pi)
#' sol_info(s)
#' # set same tolerances, e.g. for a probability
#' sol_num(0.1)
#' sol_num(0.1, tol=0.001)
#' sol_num(0.1, tol=-3)
#' # MC: Which are prime numbers?
#' prime <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
#' nonprime <- setdiff(2:30, prime)
#' # choose five false and two correct solutions
#' s <- sol_mc(nonprime, prime, sample=c(5,2), none="There are no prime numbers in the list")  
#' sol_ans(s)
#' sol_tf(s)
#' sol_info(s)
solution <- function(x, ...) { UseMethod("solution") }

#' @rdname solution
#' @export
solution.default <- function(x, ...) { sol_num(x, ...) }

#' @rdname solution
#' @export
sol_int <- function(x, tol=NA, digits=NA) {
  s <- sol_num(round(x), tol)
  s$tolerance <- if (is.na(tol)) 0.0001 else(as.numeric(tol))
  s$digits    <- if (is.na(digits)) 0 else round(as.numeric(digits))
  s
}

#' @rdname solution
#' @importFrom rstudioapi getSourceEditorContext
#' @export
sol_num <- function(x, tol=NA, digits=NA) {
  if (is.na(tol)) {
    if (length(x)>1) {
      tolerance <- min(diff(sort(x)))/2
    } else {
      tolerance <- max(0.001, 0.001*abs(x))
    }  
    digits    <- if (is.na(digits)) ceiling(-log10(tolerance)) else digits
  } else {
    tolerance <- if(tol<0) 10^tol else tol
    digits    <- if (is.na(digits)) ceiling(-log10(tolerance)) else digits
  }
  name <- knitr::current_input()  # if kniting
  if (is.null(name)) name <- parent.frame(2)$ofile # if sourcing
  if (is.null(name)) name <- try(getSourceEditorContext()$path, TRUE) # if running from RStudio
  structure(list(type="num", x=x, solution=as.character(x), digits=round(digits), tolerance=tolerance, 
                 name=name), 
            class=c("solution", "list"))
}



#' @rdname solution
#' @export
sol_mc <- function(x, y, sample=NULL, shuffle=order, none=NULL) {
  cx <- if (is.numeric(x)) fcvt(x) else as.character(x)
  cy <- if (is.numeric(y)) fcvt(y) else as.character(y)
  if (length(sample)==0) sample <- c(length(cy), length(cx))
  if (length(sample)==1) sample <- c(sample-1, 1)
  stopifnot("not enough false answers"=(sample[1]<=length(cy)),
            "not enough x answers"=(sample[2]<=length(cx)))
  sx  <- sample(length(cx), sample[1])
  sy  <- sample(length(cy), sample[2])
  ans <- c(cx[sx], cy[sy])
  atf <- rep(c(FALSE, TRUE), c(length(sx), length(sy)))
  ord <- NULL
  if (isTRUE(shuffle)) ord <- sample(length(ans))
  if (is.function(shuffle)) ord <- shuffle(c(x[sx], y[sy]))
  if (!is.null(ord)) {
    ans <- ans[ord]
    atf <- atf[ord]
  }
  if(!is.null(none)) {
    ans <- c(ans, none)
    atf <- c(atf, !any(atf))
  } 
  name <- knitr::current_input()  # if kniting
  if (is.null(name)) name <- parent.frame(2)$ofile # if sourcing
  if (is.null(name)) name <- try(getSourceEditorContext()$path, TRUE) # if running from RStudio
  structure(list(type="mchoice", answer=ans, solution=atf,
                 name=name), 
            class=c("solution", "list")) 
}

#' @rdname solution
#' @export
sol_ans <- function(x, ...) {
  stopifnot("no 'solution' object"=("solution" %in% class(x)),
            "no multiple choice"=(x$type=="mchoice"))
  if (exams::match_exams_call() %in% "exams2pdf") {
    ret <- c("", "\\begin{answerlist}", 
             paste("  \\item", x$answer), 
             "\\end{answerlist}", "")
  } else {
    ret <- c("", "Answerlist", "----------", 
             paste("*", x$answer), "")
  }
  paste0(ret, collapse="\n")
}

#' @rdname solution
#' @importFrom exams match_exams_call
#' @export
sol_tf <- function(x, ...) {
  stopifnot("no 'solution' object"=("solution" %in% class(x)),
            "no multiple choice"=(x$type=="mchoice"))
  tf <- ifelse(x$solution, "True", "False")
  if (match_exams_call() %in% "exams2pdf") {
    ret <- c("", "\\begin{answerlist}", 
             paste("  \\item", tf), 
             "\\end{answerlist}", "")
  } else {
    ret <- c("", "Answerlist", "----------", 
             paste("*", tf), "")
  }
  paste0(ret, collapse="\n")
}


#' @rdname solution
#' @export
sol_info <- function(x, ...) {
  stopifnot("no 'solution' object"=("solution" %in% class(x)))
  xds <- deparse(substitute(x))
  ret <- c("", "Meta-information", "================")
  if (x$type=="num") {
    ret <- c(ret, sprintf("extype: %s", x$type), sprintf("exsolution: %s", x$solution), 
             sprintf("extol: %s", x$tolerance))
  }
  if (x$type=="mchoice") {
    ret <- c(ret, sprintf("extype: %s", x$type), 
             sprintf("exsolution: `r mchoice2string(%s$solution)`", xds))
  }
  ret <- c(ret, sprintf("exname: %s", x$name))
  paste0(ret, collapse="\n")
}