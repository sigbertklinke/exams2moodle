#' @importFrom stats setNames
strata <- function(x, shuffle=NULL, true=NA) {
  x    <- as.character(x)
  grp  <- unique(c('TRUE', 'FALSE', x, names(shuffle)))  # all names in x and shuffle
  x    <- factor(x, levels=grp)
  nmax <- table(x)
  if (is.na(true)) true <- nmax['TRUE']>0
  true <- as.integer(true)
  #
  if (is.null(names(shuffle))) {
    if (length(shuffle)==0) shuffle <- c('TRUE'=true, 'FALSE'=length(x)-true)
    if (length(shuffle)==1) shuffle <- c('TRUE'=true, 'FALSE'=shuffle)
    shuffle <- c('TRUE'=as.integer(shuffle[1]), 'FALSE'=as.integer(sum(shuffle[-1])))
  } 
  # 
  grps <- setdiff(grp, names(shuffle))  # add zeroes if name in grp, but not in shuffle
  if (length(grps)) shuffle <- setNames(c(shuffle, rep(0, length(grps))), c(names(shuffle), grps))
  nmax    <- nmax[grp]
  shuffle <- shuffle[grp]
  if (any(shuffle>nmax)) { # extract more answers than available
    print(structure(rbind(nmax, shuffle), dimnames=list(c("Max. answers", "Answers to select"), grp)))
    stop("At least in one answer category are more answers to select than available!")
  }
  index <- list()
  for (i in 1:length(grp)) {
    grpi <- grp[i]
    if (shuffle[grpi]>0) {
      index[[grpi]] <- which(x==grpi) 
      if (length(index[[grpi]])>shuffle[grpi]) index[[grpi]] <- sample(index[[grpi]], shuffle[grpi])
    }
  }
  index <- unlist(index)
  structure(index, answer=as.character(x)[index])
}

strata ((1:5)==3, shuffle=3)

answers <- c("c", "e1", "e2", "e1", "e2", "e2")
strata(answers, shuffle=c("c"=1, "e1"=1, "e2"=2))


