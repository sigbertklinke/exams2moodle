#question <- function(expr) {
#  expr <- substitute(expr)
#  env <- new.env()
#  for(i in 1:length(expr)) eval(expr[[i]], env)
#  as.list(env)
#}
#
#res <- question({
#  library("exams2moodle")
#  x   <- ddiscrete(runif(4))
#  nx  <- 0:3
#  names(x) <- 0:3
#  y   <- ddiscrete(runif(4))
#  names(y) <- 0:3
#  xy  <- ddiscrete2(x, y)
#  sol <- 1
#})

#num <- function(x, type=c("string", "fraction"), digits=NA, latex=FALSE, inline=NA) {
#  frac <- function(f, latex) {
#    sapply(f, latex=latex, function(e, latex) {
#      if (length(e)==2) {
#        e <- if (latex) sprintf("\\frac{%s}{%s}", e[1], e[2]) else sprintf("%s / %s", e[1], e[2]) 
#      }
#      e
#    }) 
#  }
#  #
#  type <- match.arg(type)
#  if (type=="fraction") {
#    f <- attr(fractions(x), "fracs")
#    if (is.na(digits)) {
#      f <- frac(strsplit(f, '/'), latex)
#    } else {
#      f <- lapply(strsplit(f, '/'), digits=as.integer(digits), 
#                  function(e, digits) {
#                    ei <- as.integer(e)
#                    if (length(ei)==1) return(as.character(digits*c(ei, 1)))
#                    if (digits%%ei[2]==0) {
#                      e <- as.character(c(digits/ei[2]*ei[1], digits))
#                    } 
#                    e
#                  })
#      f <- frac(f, latex)
#    }
#    return(inline(f, inline))
#  }
#  return(x)
#}
#
#inline <- function(x, inline) {
#  if (is.na(inline)) return(x)
#  if (inline) return(paste0('$', x, '$'))
#  paste0('$$', x, '$$')
#}
#
#mseq <- function(x, collapse=" + ", last = collapse, inline=NA) {
#}
#msum <- function(x, ...) { mseq(x) }
#
#mobs <- function(x, index=1:length(x), obs='x', collapse=", ", last=", and ", sorted=FALSE, ...) {
#  x <- if (sorted) sprintf("x_{(%.0f)}=%s", index, x) else sprintf("x_{%.0f}=%s", index, x)
#  mseq(x, collapse=collapse, last=last, ...)  
#}