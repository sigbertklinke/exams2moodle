#' bayes_total
#'
#' Given a set of events it computes the total or conditional probability after Bayes. The events should be R names and
#' the available operators are
#' 
#' * `!` complementary event,
#' * `|` conditional event, and
#' * `^` intersection of events.
#' 
#' The attribute `compute` of the return value contains the steps for computation.
#' 
#' @param target character: target event
#' @param partition character: set of events which form a partition
#' @param ... numeric: named events with given probabilities
#'
#' @return resulting probability including the steps for computing. If `NA` is returned then no solution could be found.
#' @export
#'
#' @examples
#' bayes_total('M|T', c("M", "!M"), "M"=0.6, "T|M"=0.75, "T|!M"=0.39, 0.4)
#' pmt <- bayes_total("M|T", c("M", "!M"), "M"=0.6, "T|M"=0.75, "T|!M"=0.39, 0.4)
#' cat(paste0(attr(pmt, "compute"), "\n"))
#' #
#' bayes_total('A|K', c("A", "B", "C"), "A"=0.55, "B"=0.35, "C"=0.1, "K|A"=0.4, "K|B"=0.1, "K|C"=0.1)
#' bayes_total('K', c("A", "B", "C"), "A"=0.55, "B"=0.35, "C"=0.1, "K|A"=0.4, "K|B"=0.1, "K|C"=0.1)
bayes_total <- function(target, partition=NULL, ...) {
  notprob <- function(p) { any((p<0)|(p>1)) }
  #
  update <- function(rule, ruletype, prob, target, ...) {
    args <- list(...)
    if (ruletype=="given") { # P(A) given
      for (npi in target) {
        rule[[npi]] <- paste0("P(", npi, ")=", prob[npi])
      }
    }
    if (ruletype=="inv") { # P(A) = 1-P(B)
      for (i in seq_along(target)) {
        rule[[target[i]]] <- c(paste0("P(", target[i], ")=1-P(", args[[1]][i], ")=", 
                                      "1-", prob[args[[1]][i]], "=", prob[[target[i]]]), 
                               args[[1]][i])
      }
    }
    if (ruletype=="mul") { # P(A)=P(B)*P(C)
      for (i in seq_along(target)) { 
        rule[[target[i]]] <- c(paste0("P(", target[i], ")=P(", args[[1]][i], ")*P(", args[[2]][i], ")=",
                                      prob[args[[1]][i]], "*", prob[args[[2]][i]], "=", prob[[target[i]]]), 
                                      args[[1]][i], args[[2]][i]) 
      }
    }
    if (ruletype=="div") { # P(A)=P(B)/P(C)
      for (i in seq_along(target)) { 
        rule[[target[i]]] <- c(paste0("P(", target[i], ")=P(", args[[1]][i], ")/P(", args[[2]][i], ")=",
                                      prob[args[[1]][i]], "/", prob[args[[2]][i]], "=", prob[[target[i]]]), 
                               args[[1]][i], args[[2]][i]) 
      }
    }
    if (ruletype=="eq") { # P(A) = P(B)
      for (i in seq_along(target)) { 
        rule[[target[i]]] <- c(paste0("P(", target[i], ")=P(", args[[1]][i], ")=", prob[[target[i]]]), 
                               args[[1]][i]) 
      }
    }
    if (ruletype=="sum") {  #P(A) = sum P(A ^ partition)
      for (i in seq_along(target)) { 
        rule[[target[i]]] <- c(paste0("P(", target[i], ")=", paste0("P(", args[[1]][i,], ")", collapse="+"), 
                                      "=", paste0(prob[args[[1]][i,]], collapse="+"), "=", prob[[target[i]]]),
                               args[[1]][i,]) 
      }
    }
    rule
  }
  #
  #  stopifnot(length(partition)>1)
  args   <- list(...) 
  nargs  <- trimws(names(args))
  args   <- args[nchar(nargs)>0]
  events <- unique(unlist(lapply(nargs, function(e) { all.vars(parse(text=nargs)) } )))
  events <- c(events, paste0("!", events))
  prob   <- unlist(args)
  nprob  <- names(prob)
  rule   <- update(list(), "given", prob, nprob)
  #browser()
  # event computation
  evt        <- expand.grid(a=events, b=events, stringsAsFactors = FALSE)
  evt$given  <- paste0(evt$a, "|", evt$b)  
  evt$given2 <- paste0(evt$b, "|", evt$a)  
  evt$and    <- paste0(evt$a, "^", evt$b)  
  evt$andn   <- paste0(evt$a, "^!", evt$b)  
  evt$and2   <- paste0(evt$b, "^", evt$a)    
  lenprob    <- length(prob)
  t <- 0
  while (is.na(prob[target])) {
    # rule 1: P(!A) = 1-P(A)
    nprob  <- names(prob)
    nprob  <- nprob[!grepl('^', nprob, fixed=TRUE)]
    inprob <- ifelse(startsWith(nprob, '!'), substring(nprob, 2), paste0("!", nprob))
    index  <- which(is.na(prob[inprob]) & !is.na(prob[nprob]))
    prob[inprob[index]] <- 1-prob[nprob[index]] 
    rule   <- update(rule, "inv", prob, inprob[index], nprob[index])
    #browser()
    if (notprob(prob)) { prob[target] <- NA; break }
    # rule 2: P(A^B) = P(A|B)*P(B)
    index  <- which(is.na(prob[evt$and]) & !is.na(prob[evt$given]) & !is.na(prob[evt$b]))
    prob[evt$and[index]] <- prob[evt$given[index]]*prob[evt$b[index]]
    rule <- update(rule, "mul", prob, evt$and[index], evt$given[index], evt$b[index])
    #browser()
    if (notprob(prob)) { prob[target] <- NA; break }
    # rule 3: P(B^A) = P(A^B)
    pos   <- match(evt$and, names(prob)) 
    pos2  <- match(evt$and2, names(prob))
    index <- which(is.na(pos2) & !is.na(pos))
    prob[evt$and2[index]] <- prob[evt$and[index]]
    rule <- update(rule, "eq", prob, evt$and2[index], evt$and[index])
    #browser()
    if (notprob(prob)) { prob[target] <- NA; break }
    # rule 4: P(B|A) = P(A^B)/P(A)
    pos   <- match(evt$and, names(prob)) 
    pos2  <- match(evt$a, names(prob))
    index <- which(is.na(prob[evt$given2]) & !is.na(pos2) & !is.na(pos))
    prob[evt$given2[index]] <- prob[evt$and[index]]/prob[evt$a[index]]
    rule <- update(rule, "div", prob, evt$given2[index], evt$and[index], evt$a[index])
    #browser()
    if (notprob(prob)) { prob[target] <- NA; break }
    # rule 5: P(A) = sum P(A ^ partition)
    if (length(partition)>0) {
      sumev <- outer(events, partition, function(x,y) { paste0(x, "^", y) })
      pev   <- tapply(prob[sumev],  row(sumev), sum)
      index <- which(is.na(prob[events]) & !is.na(pev))
      prob[events[index]] <- pev[index]
      rule <- update(rule, "sum", prob, events[index], sumev[index,])
      #browser()
      if (notprob(prob)) { prob[target] <- NA; break }
    }
    #
    if (length(prob)>lenprob) lenprob <- length(prob) else break
  }
  if (is.na(prob[target])) return(NA)
  #browser()
  step <- target
  sel  <- rep(FALSE, length(rule))
  nsel <- names(rule)
  while (length(step)) {
    sel[which(step[1]==nsel)] <- TRUE
    step <- c(step[-1], rule[[step[1]]][-1])
  }
  compute <- sapply(rule[sel], '[[', 1)
  latex   <- sub("=", "&=", compute, fixed=TRUE)
  latex   <- gsub("^", "\\cap{}", latex, fixed=TRUE)
  latex   <- gsub("*", "\\cdot{}", latex, fixed=TRUE)
  structure(prob[target], compute=compute, latex=c("\\begin{align*}", paste0(latex, "\\\\"), "\\end{align*}"))
}