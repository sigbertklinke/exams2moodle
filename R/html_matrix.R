#' @rdname html_matrix
#' @title html_matrix
#' @description Creates from a vector, matrix, array,  or table a HTML representation of it. The HTML representation has one column and row more 
#' than the data. The additional row and column are used to have a title (top left), the column names (top), and the row names (left).
#' 
#' You can set the style attributes (`<td style="...">`) via `hm_cell`, `hm_title`, `hm_col`, and `hm_row`. 
#' For example: `hm_cell(hm, 1, 1, text_align="right")` will lead to (`<td style="text-align:right;">`) for the cell (1,1) and any
#' unnamed element will change the cell value. 
#' Note: since `-` is an operator in R, we use `_` instead. Of course, you could use `"text-align"="right"`, but I'am lazy.
#' 
#' @param x vector, matrix, array, table or html_matrix: input
#' @param byrow logical: create a row or column matrix if `x` is one-dimensional (default: \code{FALSE})
#' @param numeric list: list of HTML style properties for a cell if `class(x[i,j])=="numeric"` (default: \code{list(text_align="right")})
#' @param integer list: list of HTML style properties for a cell if `class(x[i,j])=="integer"` (default: \code{list(text_align="right")})
#' @param char list: list of HTML style properties for a cell if `class(x[i,j])=="character"` (default: \code{list(text_align="left")})
#' @param logical list: list of HTML style properties for a cell if `class(x[i,j])=="logical"` (default: \code{list(text_align="right")})
#' @param border character: vector of background color for a border cell (default: \code{"#999999")})
#' @param ... further parameters
#'
#' @return `html_matrix` returns a html_matrix, `print` returns invisible a character matrix 
#' @export
#'
#' @examples
#' m <- matrix(1:6, ncol=2)
#' m
#' l <- html_matrix(m)
#' l
html_matrix <- function(x, ...) { UseMethod("html_matrix") }

#' @rdname html_matrix
#' @export
html_matrix.default <- function(x, ..., byrow=FALSE, 
                                numeric=list(text_align="right"), 
                                integer=list(text_align="right"), 
                                char=list(text_align="left"),
                                logical=list(text_align="right"),
                                border="#999999") {
  title <- deparse(substitute(x))
  val   <- as.list(x) 
  dim   <- attr(x, "dim")
  rownames <- colnames <- NULL
  if (length(dim)<2) {
    if (byrow) { 
      dim <- c(length(val), 1) 
      colnames <- names(x)
    } else {
      dim <- c(1, length(val))    
      rownames <- names(x)
    }
  } else {
    if (length(dim)>2) dim <- c(dim[1], prod(dim[-1]))
    colnames <- colnames(x)
    rownames <- rownames(x)
  }
  if (is.null(colnames)) colnames <- sprintf("[,%.0f]", seq(dim[2]))
  if (is.null(rownames)) rownames <- sprintf("[%.0f,]", seq(dim[1]))
  cols <- vector("list", length(colnames))
  for (i in seq(colnames)) cols[[i]] <- list(value=colnames[i], fmt="%s", text_align="right", background_color=border, vertical_align="top", font_weight="bold", min_width="60px")
  rows <- vector("list", length(rownames))
  for (i in seq(rownames)) rows[[i]] <- list(value=rownames[i], fmt="%s", text_align="left", background_color=border, vertical_align="top", font_weight="bold")
  ret  <- vector("list", length(val))
  for (i in seq(val)) {
    reti <- list()
    if("integer" %in% class(val[[i]])) {
      reti <- numeric
      if (is.null(reti$fmt)) reti$fmt <- "%.0f"
    }
    if("numeric" %in% class(val[[i]])) {
      reti <- numeric
      if (is.null(reti$fmt)) reti$fmt <- "%.3f"
    }
    if("logical" %in% class(val[[i]])) {
      reti <- logical
      if (is.null(reti$fmt)) reti$fmt <- "%.0f"
    }
    if("character" %in% class(val[[i]])) {
      reti <- char
      if (is.null(reti$fmt)) reti$fmt <- "%s"
    }
    reti$value <- val[[i]]
    ret[[i]]   <- reti
  }
  #
  attr(ret, "title")    <- list(value=title, fmt="%s", text_align="left", background_color=border, vertical_align="top", font_weight="bold")
  attr(ret, "rownames") <- rows
  attr(ret, "colnames") <- cols
  attr(ret, "tr") <- vector("list", 1+length(rows))
  attr(ret, "table") <- list()
  structure(ret, dim=dim, class=c("html_matrix", class(ret)))
}

hm_pos <- function(nrow, ...) {
  #browser()
  args   <- list(...)
  pos <- matrix(NA_integer_, nrow=nrow, ncol=length(args))
  for (j in seq(args)) pos[,j] <- rep(1:length(args[[j]]), length.out=nrow) 
  nargs <- names(args)
  if (is.null(nargs)) nargs <- rep('', length(args))
  nargs[nargs==''] <- 'value'
  list(argpos=pos, nargs=nargs)
}

#' @rdname hm_cell
#' @title hm_cell
#' @aliases hm_index hm_row hm_col hm_title
#' @description
#' * `hm_cell` or `hm_index` modify a data cell format (`fmt="%s"`), value (unnamed parameter) or style (`text_align="left"`)
#' * `hm_col` or `hm_row` modify a row or column format (`fmt="%s"`), value (unnamed parameter) or style (`text_align="left"`)
#' @param x html_matrix object
#' @param row integer: row(s) to access
#' @param col integer: column(s) to access
#' @param ind integer vector or matrix: access various (row and columns) elements (first column: row, second column: column)
#' @param byrow logical: order indices by row or column (default: \code{FALSE})
#' @param ... elements to change
#'
#' @md
#' @return modified html_matrix object
#' @export
#'
#' @examples
#' l <- html_matrix(matrix(1:6, ncol=2))
#' # replace l[1,1] by NA
#' hm_cell(l, 1, 1, NA)
#' # replace l[1,1] by NA and set the text_align to center
#' hm_cell(l, 1, 1, NA, text_align="center")
#' # replace l[1,3] and l[2,1] by NA
#' rcind <- cbind(c(1,3), c(2, 1))
#' hm_index(l, rcind, NA)
#' # set a new title
#' hm_title(l, "new title")
#' # set a new row or column title
#' hm_row(l, 2, "row 2")
#' hm_col(l, 1, "col 1")
#' # set fmt by column or row
#' print(hm_cell(l, fmt=c("%.0f", "%.1f", "%.2f"), byrow=FALSE), which="fmt")
#' print(hm_cell(l, fmt=c("%.0f", "%.1f"), byrow=TRUE), which="fmt")
hm_cell  <- function(x, row=NULL, col=NULL, ..., byrow=FALSE) { 
  #browser()
  stopifnot("html_matrix" %in% class(x))
  if (is.null(row)) row <- seq(nrow(x))
  if (is.null(col)) col <- seq(ncol(x))
  ind <- expand.grid(row, col)
  ind <- if (byrow) ind[order(ind[,1], ind[,2]),] else ind[order(ind[,2], ind[,1]),]
  hm_index (x, ind, ...)
}

#' @rdname hm_cell
#' @export
hm_index <- function(x, ind, ...)    {
  stopifnot("html_matrix" %in% class(x))
  stopifnot(length(dim(x))==2)
  pos  <- hm_pos(nrow=nrow(ind), ...)
  args <- list(...)
  for (i in 1:nrow(ind)) {
    for (j in seq(args)) {
      x[[ind[i,1], ind[i,2]]][[pos$nargs[j]]] <- args[[j]][pos$argpos[i,j]]
    }
  }
  x
}

#' @rdname hm_cell
#' @export
hm_title <- function(x, ...) {
  stopifnot("html_matrix" %in% class(x))
  pos   <- hm_pos(nrow=1, ...)
  args  <- list(...)
  param <- attr(x, "title")
  for (j in seq(args)) {
    param[[pos$nargs[j]]] <- args[[j]][pos$argpos[1,j]]
  }
  attr(x, "title") <- param
  x
}    

#' @rdname hm_cell
#' @export
hm_table <- function(x, ...) {
  stopifnot("html_matrix" %in% class(x))
  pos   <- hm_pos(nrow=1, ...)
  args  <- list(...)
  param <- attr(x, "table")
  for (j in seq(args)) {
    param[[pos$nargs[j]]] <- args[[j]][pos$argpos[1,j]]
  }
  attr(x, "table") <- param
  x
}    
      
#' @rdname hm_cell
#' @export
hm_row <- function(x, ind, ...)  {
  stopifnot("html_matrix" %in% class(x))
  pos   <- hm_pos(nrow=length(ind), ...)
  args  <- list(...)
  param <- attr(x, "rownames")
  for (i in 1:length(ind)) {
    for (j in seq(args)) {
      param[[i]][[pos$nargs[j]]] <- args[[j]][pos$argpos[i,j]]
    }
  }
  attr(x, "rownames") <- param
  x
}

#' @rdname hm_cell
#' @export
hm_col <- function(x, ind, ...) {
  stopifnot("html_matrix" %in% class(x))
  pos   <- hm_pos(nrow=length(ind), ...)
  args  <- list(...)
  param <- attr(x, "colnames")
  for (i in 1:length(ind)) {
    for (j in seq(args)) {
      param[[i]][[pos$nargs[j]]] <- args[[j]][pos$argpos[i,j]]
    }
  }
  attr(x, "colnames") <- param
  x
}

#' @rdname hm_cell
#' @export
hm_tr <- function(x, ind, ...) {
  stopifnot("html_matrix" %in% class(x))
  pos   <- hm_pos(nrow=length(ind), ...)
  args  <- list(...)
  param <- attr(x, "tr")
  for (i in 1:length(ind)) {
    for (j in seq(args)) {
      param[[i]][[pos$nargs[j]]] <- args[[j]][pos$argpos[i,j]]
    }
  }
  attr(x, "tr") <- param
  x
}