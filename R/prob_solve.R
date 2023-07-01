#' @rdname prob_solve
#' @title prob_solve
#' @description The follwing functions are available
#' 
#' * `prob_solve` given a set of events it computes the total or conditional probability of the given event or 
#' `NA` if no solution could be found. For the name of the events upper case letters must be used and 
#' the available operators are `!` (complementary event), `|` (conditional event), and `^` (intersection of events). 
#' The attribute `latex` of the return value contains the necessary computation steps for computation of the given event.
#' If `getprob` is `TRUE` then additionally the attribute `prob`, a vector with all computed probabilities, and `compute`
#' with all computation steps are generated.
#' * `print` shows the solution way in ASCII.
#' * `toLatex` shows the solution way in LaTeX/MathJax with an `align*` environment.
#' * `lprob` converts `!A` to `\\bar{A}` and `A^B` to `A \\cap B`.
#' 
#' @details The program applies iteratively the following rules to find a solution:
#' 
#' * \eqn{P(A) = 1-P(!A)},
#' * \eqn{P(A|B) = 1-P(!A|B)},
#' * \eqn{P(A^B) = P(B^A)},
#' * \eqn{P(B) = P(A^B)+P(!A^B)},
#' * \eqn{P(A|B) = P(A^B)/P(B)}, and
#' * \eqn{P(A) = P(A|P1)+P(A|P2)+...+ P(A|Pn)} for a partition \eqn{P1, P2, ..., Pn}.
#' 
#' @param target character: target event
#' @param ... numeric: named events with given probabilities
#' @param partition character or list: set of events which form a partition
#' @param quiet logical: show all computation steps (default: `FALSE`)
#' @param getprob logical: return all computed probabilities and used computation steps (default: `FALSE`)
#' @param object,x prob_solve object
#' @param type character: what to print, either `numeric` (solution, default), `latex` (solution steps in ASCII format), 
#' `prob` (optional: all probabilities computed), `compute` (optional: all rules used).
#' @param txt character: vector to convert `!` to `\\bar` and `^` to `\\cap'.
#' 
#' @return An object of class `prob_solve` with the resulting probability including the steps for computing. 
#' If `NA` is returned then no solution could be found.
#' @export
#'
#' @examples
#' prob_solve("!A", "A"=0.3)
#' prob_solve("!A|B", "A|B"=0.3)
#' prob_solve("B^A", "A^B"=0.3)
#' # P(B)   = P(A^B)+P(!A^B)
#' prob_solve("B", "A^B"=0.3, "!A^B"= 0.4)
#' prob_solve("A^B", "B"=0.7, "!A^B"= 0.4)
#' prob_solve("!A^B", "B"=0.7, "A^B"= 0.3)
#' # P(A|B) = P(A^B)/P(B)
#' prob_solve("A|B", "A^B"=0.3, "B"= 0.6)
#' prob_solve("A^B", "B"=0.6, "A|B"= 0.5)
#' prob_solve("B", "A|B"=0.5, "A^B"= 0.3)
#' #' latex, prob and compute attributes
#' pmt <- prob_solve("M|T", "M"=0.6, "T|M"=0.75, "T|!M"=0.39, quiet=FALSE, getprob=TRUE)
#' toLatex(pmt)
#' attr(pmt, "latex")
#' pmt <- prob_solve("M|T", "M"=0.6, "T|M"=0.75, "T|!M"=0.39, quiet=FALSE, getprob=TRUE)
#' attr(pmt, "prob")
#' print(pmt, "latex") 
#' print(pmt, "prob")    # only if getprob=TRUE   
#' print(pmt, "compute") # only if getprob=TRUE   
#' # bayes theorem and total probability
#' prob_solve("Z", "Z|A"=0.1, "Z|B"=0.2, "Z|C"=0.3, partition=c("A", "B", "C"))
#' prob_solve("Z|A", "Z"=0.6, "Z|B"=0.2, "Z|C"=0.3, partition=c("A", "B", "C"))
#' prob_solve('A|K', "A"=0.55, "B"=0.35, "C"=0.1, "K|A"=0.4, "K|B"=0.1, "K|C"=0.1, 
#'            partition=c("A", "B", "C"))
#' prob_solve('K', "A"=0.55, "B"=0.35, "C"=0.1, "K|A"=0.4, "K|B"=0.1, "K|C"=0.1, 
#'            partition=c("A", "B", "C"))
prob_solve <- function(target, ...) { UseMethod("prob_solve") }

#' @rdname prob_solve
#' @export
prob_solve.default <- function(target, ..., partition = NULL, getprob=FALSE, quiet=TRUE) {
  getProb <- function(name, prob) {
    value <- matrix(NA, ncol=ncol(name), nrow=nrow(name))
    for (i in 1:ncol(value)) value[,i] <- prob[name[,i]]
    value                
  }
  #
  lkeep  <- function(event) { sapply(strsplit(event, ''), function(v) { sum(LETTERS %in% v)==2 } )}
  lnot   <- function(event) { 
    if(length(event)) event <- gsub("!!", "", paste0("!", event)) 
    event
  }
  #
  lcond  <- function(a, b)  { 
    ret <- as.character(outer(a, b, function(x,y) { paste0(x,'|', y) }))
    ret[lkeep(ret)]
  }
  #
  land   <- function(a, b, swap=FALSE)  { 
    if (swap) 
      ret <- as.character(outer(a, b, function(x,y) { paste0(y,'^', x) })) 
    else 
      ret <- as.character(outer(a, b, function(x,y) { paste0(x,'^', y) })) 
    ret[lkeep(ret)]
  }
  #
  lleft  <- function(a, b)  { 
    keep <- lkeep(as.character(outer(a, b, function(x,y) { paste0(y,'^', x) })))
    ret <- as.character(outer(a, b, function(x,y) { x }))
    ret[keep]
  }
  #
  lright <- function(a, b)  { 
    keep <- lkeep(as.character(outer(a, b, function(x,y) { paste0(y,'^', x) })))
    ret <- as.character(outer(a, b, function(x,y) { y }))
    ret[keep]
  }  
  prob        <- unlist(list(...))
  names(prob) <- gsub("\\..*$", "", names(prob))
  toks  <- unique(unlist(strsplit(names(prob), "")))
  stopifnot(all(toks %in% c(LETTERS, '!', '^', '|')))
  event    <- setdiff(toks, c('!', '^', '|'))
  compstep <- NULL
  for (np in names(prob)) compstep <- c(compstep, sprintf("P(%s)& = %s", np, fcvt(prob[np])))
  # create possible rules
  aevt   <- c(event, lnot(event))
  rule   <- list(not=cbind(aevt, lnot(aevt)),
                 notcond=cbind(lcond(aevt, aevt), lnot(lcond(aevt, aevt))),
                 andsym=cbind(land(aevt, aevt), land(aevt, aevt, TRUE)),
                 andadd=cbind(lright(aevt, aevt), land(aevt, aevt), lnot(land(aevt, aevt))),
                 cond=cbind(lcond(aevt, aevt), land(aevt, aevt), lright(aevt, aevt))
  )
  nrule <- length(rule)
  if (!is.null(partition)) {
    if (!is.list(partition)) partition <- list(partition)
    aevt <- event
    for (i in 1:length(partition)) aevt <- setdiff(aevt, partition[[i]])
    for (i in 1:length(partition)) {
      parti <- aevt
      for (j in 1:length(partition[[i]])) parti <- cbind(parti, paste0(aevt, '|', partition[[i]][[j]]))
      rule[[length(rule)+1]] <- parti
    }
  }
  #
  repeat {
    lprob  <- length(prob)
    # apply rule P(A) = 1-P(!A)
    rprob <- getProb(rule$not, prob)
    index <- which(rowSums(is.na(rprob))==1)
    if (length(index)) {
      for (i in index) {
        if (is.na(prob[rule$not[i,1]])) {
          prob[rule$not[i,1]] <- 1-rprob[i,2]
          step <- sprintf("P(%s)& = 1-P(%s) = 1-%s = %s", rule$not[i,1],  rule$not[i,2], 
                          fcvt(rprob[i,2]), fcvt(prob[rule$not[i,1]]))
          compstep <- c(compstep, step)
          if (!quiet) cat('11:', step, "\n")
        } 
        if (is.na(prob[rule$not[i,2]])) {
          prob[rule$not[i,2]] <- 1-rprob[i,1]
          step  <- sprintf("P(%s)& = 1-P(%s) = 1-%s = %s", rule$not[i,2],  rule$not[i,1], 
                           fcvt(rprob[i,1]), fcvt(prob[rule$not[i,2]]))
          compstep <- c(compstep, step)          
          if (!quiet) cat('12:', step, "\n")
        }
      }
    }
    # apply rule P(A|B) = 1-P(!A|B)
    rprob <- getProb(rule$notcond, prob)
    index <- which(rowSums(is.na(rprob))==1)
    if (length(index)) {
      for (i in index) {
        if (is.na(prob[rule$notcond[i,1]])) {
          prob[rule$notcond[i,1]] <- 1-rprob[i,2]
          step <- sprintf("P(%s)& = 1-P(%s) = 1-%s = %s", rule$notcond[i,1],  rule$notcond[i,2], 
                          fcvt(rprob[i,2]), fcvt(prob[rule$notcond[i,1]]))
          compstep <- c(compstep, step)
          if (!quiet) cat('21:', step, "\n")
        } 
        if (is.na(prob[rule$notcond[i,2]])) {
          prob[rule$notcond[i,2]] <- 1-rprob[i,1]
          step <- sprintf("P(%s)& = 1-P(%s) = 1-%s = %s", rule$notcond[i,2],  rule$notcond[i,1], 
                          fcvt(rprob[i,1]), fcvt(prob[rule$notcond[i,2]]))
          compstep <- c(compstep, step)
          if (!quiet) cat('22:', step, "\n")
        }
      }
    }
    # apply rule P(A^B) = P(B^A)
    rprob <- getProb(rule$andsym, prob)
    index <- which(rowSums(is.na(rprob))==1)
    if (length(index)) {
      for (i in index) {
        if (is.na(prob[rule$andsym[i,1]])) {
          prob[rule$andsym[i,1]] <- rprob[i,2]
          step <-  sprintf("P(%s)& = P(%s) = %s", rule$andsym[i,1], rule$andsym[i,2], fcvt(rprob[i,2]))
          compstep <- c(compstep, step)
          if (!quiet) cat('31:', step, "\n")
        } 
        if (is.na(prob[rule$andsym[i,2]])) {
          prob[rule$andsym[i,2]] <- rprob[i,1]
          step <- sprintf("P(%s)& = P(%s) = %s", rule$andsym[i,2], rule$andsym[i,1], fcvt(rprob[i,1]))   
          compstep <- c(compstep, step)
          if (!quiet) cat('32:', step, "\n")
        }
      }
    }
    # apply rule P(B)   = P(A^B)+P(!A^B)
    rprob <- getProb(rule$andadd, prob)
    index <- which(rowSums(is.na(rprob))==1)
    if (length(index)) {    
      for (i in index) {
        if (is.na(prob[rule$andadd[i,1]])) {
          prob[rule$andadd[i,1]] <- rprob[i,2]+rprob[i,3]
          step <- sprintf("P(%s)& = P(%s)+P(%s) = %s+%s = %s", 
                          rule$andadd[i,1], rule$andadd[i,2], rule$andadd[i,3],  
                          fcvt(rprob[i,2]), fcvt(rprob[i,3]), prob[rule$andadd[i,1]])
          compstep <- c(compstep, step)
          if (!quiet) cat('41:', step, "\n")
        }
        if (is.na(prob[rule$andadd[i,2]])) {
          prob[rule$andadd[i,2]] <- rprob[i,1]-rprob[i,3]          
          step <- sprintf("P(%s)& = P(%s)-P(%s) = %s-%s = %s", 
                          rule$andadd[i,2], rule$andadd[i,1], rule$andadd[i,3],  
                          fcvt(rprob[i,1]), fcvt(rprob[i,3]), prob[rule$andadd[i,2]])
          compstep <- c(compstep, step)
          if (!quiet) cat('42:', step, "\n")
        }
        if (is.na(prob[rule$andadd[i,3]])) {
          prob[rule$andadd[i,3]] <- rprob[i,1]-rprob[i,2]          
          step <- sprintf("P(%s)& = P(%s)-P(%s) = %s-%s = %s", 
                          rule$andadd[i,3], rule$andadd[i,1], rule$andadd[i,2],  
                          fcvt(rprob[i,1]), fcvt(rprob[i,2]), prob[rule$andadd[i,3]])
          compstep <- c(compstep, step)
          if (!quiet) cat('43:', step, "\n")
        }
      }
    }
    # apply rule P(A|B) = P(A^B)/P(B)
    rprob <- getProb(rule$cond, prob)
    index <- which(rowSums(is.na(rprob))==1)
    if (length(index)) {    
      for (i in index) {
        if (is.na(prob[rule$cond[i,1]])) {
          prob[rule$cond[i,1]] <- rprob[i,2]/rprob[i,3]
          step <- sprintf("P(%s)& = \\frac{P(%s)}{P(%s)} = \\frac{%s}{%s} = %s", 
                          rule$cond[i,1], rule$cond[i,2], rule$cond[i,3],  
                          fcvt(rprob[i,2]), fcvt(rprob[i,3]), prob[rule$cond[i,1]])
          compstep <- c(compstep, step)
          if (!quiet) cat('51:', step, "\n")
        }
        if (is.na(prob[rule$cond[i,2]])) {
          prob[rule$cond[i,2]] <- rprob[i,1]*rprob[i,3]          
          step <-  sprintf("P(%s)& = P(%s) \\cdot P(%s) = %s \\cdot %s = %s", 
                           rule$cond[i,2], rule$cond[i,1], rule$cond[i,3],  
                           fcvt(rprob[i,1]), fcvt(rprob[i,3]), prob[rule$cond[i,2]])
          compstep <- c(compstep, step)
          if (!quiet) cat('52:', step, "\n")
        }
        if (is.na(prob[rule$cond[i,3]])) {
          prob[rule$cond[i,3]] <- rprob[i,2]/rprob[i,1]          
          step <-  sprintf("P(%s)& = \\frac{P(%s)}{P(%s)} = \\frac{%s}{%s} = %s", 
                           rule$cond[i,3], rule$cond[i,2], rule$cond[i,1],  
                           fcvt(rprob[i,2]), fcvt(rprob[i,1]), prob[rule$cond[i,3]])
          compstep <- c(compstep, step)
          if (!quiet) cat('53', step, "\n")
        }
      }
    }
    # apply rule P(A) = P(A|P1)+P(A|P2)+...+ P(A|Pn)
    if (length(rule)>nrule) {
      #browser()
      for (j in (nrule+1):length(rule)) {
        rprob <- getProb(rule[[j]], prob)
        index <- which(rowSums(is.na(rprob))==1)
        if (length(index)) {    
          for (i in index) {
            k <- which(is.na(rprob[i,]))
            if (is.na(prob[rule[[j]][i,1]])) {
              prob[rule[[j]][i,1]] <- sum(rprob[i,-1])
              fj <- sprintf("P(%s)& = %s", rule[[j]][i,1], paste0(sprintf("P(%s)", rule[[j]][i,-1]), collapse="+"))
              vj <- paste0(sprintf("%s", fcvt(rprob[i,-1])), collapse="+")
              step <- sprintf("%s = %s = %s", fj, vj, fcvt(prob[rule[[j]][i,1]]))
              compstep <- c(compstep, step)
              if (!quiet) cat('91:', step, "\n") 
            } 
            if ((k>1) && is.na(prob[rule[[j]][i,k]])) {
              prob[rule[[j]][i,k]] <- rprob[i,1]-sum(rprob[i,-c(1,k)])    
              fj <- sprintf("P(%s)& = P(%s)-%s", rule[[j]][i,k], rule[[j]][i,1], 
                            paste0(sprintf("P(%s)", rule[[j]][i,-c(1,k)]), collapse="-"))
              vj <- paste0(fcvt(rprob[i,1]), "-", paste0(sprintf("%s", fcvt(rprob[i, -c(1,k)])), collapse="-")) 
              step <- sprintf("%s = %s = %s", fj, vj, fcvt(prob[rule[[j]][i,k]]))
              compstep <- c(compstep, step)
              if (!quiet) cat('99:', step, "\n")
            }
          }
        }
      }
    }
    #browser()
    if (!is.na(prob[target]) || (length(prob)==lprob)) break
  }
  #browser()
  latex <- NULL
  value <- prob[target]
  if (!is.na(value)) {
    evts  <- lapply(str_match_all(compstep, "\\((.*?)\\)"), function(v) { v[,2]})
    evts1 <- sapply(evts, '[', 1)
    build <- target
    done  <- NULL
    while(length(build)) {
      pos    <- which(evts1==build[1])
      latex  <- c(latex, compstep[pos])
      done   <- c(done, build[1])
      build  <- setdiff(c(build[-1], evts[[pos]][-1]), done)
    }
  }
  structure(value, latex=rev(latex), names=target, class="prob_solve",
            prob=if(getprob) prob else NULL, compute=if(getprob) compstep else NULL)
}

#' @rdname prob_solve
#' @export
lprob <- function(txt) {
  ret <- gsub("^", " \\cap ", txt, fixed=TRUE)
  gsub("!([A-Z]{1})", "\\\\bar{\\1}", ret)
}

#' @rdname prob_solve
#' @export
toLatex.prob_solve <- function(object, ...) {
  ret <- lprob(attr(object, "latex"))
  c("\\begin{align*}", paste(ret, " \\\\"), "\\end{align*}")
}

#' @rdname prob_solve
#' @importFrom stringr str_pad
#' @export
print.prob_solve <- function(x, type=c("numeric", "latex", "prob", "compute"), ...) {
  align <- function(lines) {
    lines  <- strsplit(lines, "=", fixed=TRUE)
    ncharm <- max(sapply(lines, function(e) { nchar(e[1]) }))
    sapply(lines, function(e) {
      if (nchar(e[1]<ncharm)) e[1] <- str_pad(e[1], ncharm, 'right')
      paste0(e, collapse="=")
    })
  }
  #
  type <- match.arg(type)
  if (type=="numeric") {
    ret <- as.numeric(x)
    print(ret)
  }
  if (type=="latex") {
    ret <- gsub("&", "", attr(x, "latex"), fixed=TRUE)     
    ret <- align(ret)
    cat(paste0(ret, collapse="\n"))
  }
  if (type=="prob") {
     ret <- attr(x, "prob")
     print(ret)     
  }
  if (type=="compute") {
    ret <- gsub("&", "", attr(x, "compute"), fixed=TRUE)     
    ret <- align(ret)
    cat(paste0(ret, collapse="\n"))
  }
  invisible(ret)
}