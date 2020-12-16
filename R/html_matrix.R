#' html_matrix
#'
#' Returns a HTML representation of a matrix with the following parameters and defaults (Note that recycling can used with most parameters):
#' \describe{
#' \item{\code{title}}{entry at the top left (default: \code{""})}
#' \item{\code{caption}}{entry for the caption (default: \code{""})}
#' \item{\code{names$col}}{entry for the caption (default: \code{colnames(x)})}
#' \item{\code{names$row}}{entry for the caption (default: \code{rownames(x)})}
#' \item{\code{style$table}}{style for the table (default: \code{""})}
#' \item{\code{style$caption}}{style for the caption (default: \code{""})}
#' \item{\code{style$title}}{style for the caption (default: \code{"background-color:#999999;vertical-align:top;text-align:left;font-weight:bold;"})}
#' \item{\code{style$row}}{style for the row names (default: \code{"background-color:#999999;vertical-align:top;text-align:right;font-weight:bold;"})}
#' \item{\code{style$col}}{style for the col names (default: \code{"background-color:#999999;vertical-align:top;text-align:right;font-weight:bold;"})}
#' \item{\code{style$cell}}{style for the col names (default: \code{c("background-color:#CCCCCC; vertical-align:top; text-align:right;", "background-color:#FFFFFF; vertical-align:top; text-align:right;")})}
#' \item{\code{style$logical}}{style for a logical matrix entry (default: \code{c("background-color:#CCCCCC; vertical-align:top; text-align:right;", "background-color:#FFFFFF; vertical-align:top; text-align:right;")})}
#' \item{\code{style$numeric}}{style for a numeric matrix entry (default: \code{c("background-color:#CCCCCC; vertical-align:top; text-align:right;", "background-color:#FFFFFF; vertical-align:top; text-align:right;")})}
#' \item{\code{style$char}}{style for a numeric matrix entry (default: \code{c("background-color:#CCCCCC; vertical-align:top; text-align:right;", "background-color:#FFFFFF; vertical-align:top; text-align:left;"})}
#' \item{\code{format$title}}{\code{fmt} parameter to format the title via [base::sprintf] (default: \code{"\%s"})}
#' \item{\code{format$row}}{\code{fmt} parameter to format the row names via [base::sprintf] (default: \code{"\%s"})}
#' \item{\code{format$col}}{\code{fmt} parameter to format the col names via [base::sprintf] (default: \code{"\%s"})}
#' \item{\code{format$cell}}{\code{fmt} parameter to format a matrix entry via [base::sprintf]}
#' \item{\code{format$logical}}{\code{fmt} parameter to format a logical matrix entry via [base::sprintf] (default: \code{"\%d"})}
#' \item{\code{format$numeric}}{\code{fmt} parameter to format a numeric matrix entry via [base::sprintf] (default: \code{"\%f"})}
#' \item{\code{format$char}}{\code{fmt} parameter to format a character matrix entry via [base::sprintf] (default: \code{"\%s"})}
#' }
#'
#' @param x matrix: matrix to print in HTML
#' @param title character: matrix title, the left upper entry of the matrix (default: \code{NULL}) 
#' @param caption character: caption (default: \code{NULL}) 
#' @param format list: list of parameters to format matrix entries (default: \code{list()})
#' @param style list: list of parameters to set the style parameters for each matrix entry (default: \code{list()})
#' @param names list: list of row- and/or colnames (default: \code{list()})
#' @param type character: output format, either \code{"html"}, \code{"wiki"} or \code{"plain"}  (default: \code{"html"})
#'
#' @return Plain text, HTML or MediaWiki code
#' @md
#' @export
#'
#' @examples
#' x <- matrix(runif(12), ncol=3)
#' html_matrix(x)
html_matrix <- function(x, title=NULL, caption=NULL, format=list(), style=list(), names=list(), type="html") {
  getData <- function(x) {
    data <- NULL;
    coln <- colnames(x);
    rown <- rownames(x);
    if (is.data.frame(x)) {
      data <- as.list(x);
      if (is.null(coln)) coln <-sprintf("V%i", 1:ncol(x)); 
      if (is.null(rown)) rown <-sprintf("%i",  1:nrow(x)); 
    }
    if (is.matrix(x)) { 
      data <- lapply(seq_len(ncol(x)), function(i) x[,i]);
      if (is.null(coln)) coln <-sprintf("[,%i]", 1:ncol(x)); 
      if (is.null(rown)) rown <-sprintf("[%i,]", 1:nrow(x)); 
    }
    if (is.null(data)) {
      data <- as.matrix(x)
      if (is.null(coln)) coln <-sprintf("[,%i]", 1:ncol(data)); 
      if (is.null(rown)) rown <-sprintf("[%i,]", 1:nrow(data)); 
      data <- lapply(seq_len(ncol(data)), function(i) data[,i]);
    }
    if (is.null(data)) stop("two dimensional array or data frame expected");
    return(list(data=data, colnames=coln, rownames=rown));  
  }
  #
  recycle <- function (vec, max) { return (1+(vec-1)%%max); }
  #
  gd = getData(x);
  # defaults and check
  param                = list();
  param$title          = as.character(if (is.null(title)) "" else title);
  param$caption        = as.character(if (is.null(caption)) "" else caption);
  param$col            = as.vector(if (is.null(names$col)) gd$colnames else names$col, "character");
  param$row            = as.vector(if (is.null(names$row)) gd$rownames else names$row, "character");  
  param$style.table    = as.character(if (is.null(style$table)) "" else style$table);
  param$style.caption  = as.character(if (is.null(style$caption)) "" else style$caption); 
  param$style.title    = as.character(if (is.null(style$caption)) "background-color:#999999;vertical-align:top;text-align:left;font-weight:bold;" else style$caption);
  param$style.row      = as.vector(if (is.null(style$row)) "background-color:#999999;vertical-align:top;text-align:right;font-weight:bold;" else style$row);
  param$style.col      = as.vector(if (is.null(style$col)) "background-color:#999999;vertical-align:top;text-align:right;font-weight:bold;" else style$col);
  param$style.logical  = as.matrix(if (is.null(style$logical)) c("background-color:#CCCCCC; vertical-align:top; text-align:right;", "background-color:#FFFFFF; vertical-align:top; text-align:right;") else style$logical);
  param$style.numeric  = as.matrix(if (is.null(style$numeric)) 
    if (is.null(style$cell)) c("background-color:#CCCCCC; vertical-align:top; text-align:right;", "background-color:#FFFFFF; vertical-align:top; text-align:right;") 
    else style$cell
    else style$numeric);
  param$style.char     = as.matrix(if (is.null(style$char)) c("background-color:#CCCCCC; vertical-align:top; text-align:right;", "background-color:#FFFFFF; vertical-align:top; text-align:left;") else style$char);
  param$format.title   = as.character(if (is.null(format$title)) "%s" else format$title);                             
  param$format.row     = as.vector(if (is.null(format$row)) "%s" else format$row, "character");                             
  param$format.col     = as.vector(if (is.null(format$col)) "%s" else format$col, "character");                             
  param$format.logical = as.matrix(if (is.null(format$logical)) "%d" else format$logical);
  param$format.numeric = as.matrix(if (is.null(format$numeric)) 
    if (is.null(format$cell)) "%f" else format$cell
    else format$numeric);
  param$format.char    = as.matrix(if (is.null(format$char)) "\"%s\"" else format$char);
  # format data                                    
  cols   <- length(gd$data);
  colv   <- 1:cols;
  rows   <- length(gd$data[[1]]);
  rowv   <- 1:rows;
  output <-  ostyle <-matrix("", nrow=1+rows, ncol=1+cols);
  ## title    
  output[1,1] <- sprintf(param$format.title, param$title);
  ostyle[1,1] <- param$style.title;
  ## column headers
  pos  <- recycle(colv, length(param$col));                     
  posf <- recycle(colv, length(param$format.col)); 
  poss <- recycle(colv, length(param$style.col));
  output[1,1+colv] <- sprintf(param$format.col[posf], param$col[pos]);
  ostyle[1,1+colv] <- param$style.col[poss];
  ## row headers
  pos  <- recycle(rowv, length(param$row));                     
  posf <- recycle(rowv, length(param$format.row)); 
  poss <- recycle(rowv, length(param$style.row));
  output[1+rowv,1] <- sprintf(param$format.row[posf], param$row[pos]);
  ostyle[1+rowv,1] <- param$style.row[poss];
  ## table
  for (i in colv) {
    di   <- gd$data[[i]];
    niv  <- 1:length(di);
    typei <- typeof(di[1]);
    if (typei=="logical") {
      posf <- recycle(niv, nrow(param$format.logical));         
      posi <- recycle(colv, ncol(param$format.logical))[i];         
      output[1+rowv,i+1] <- sprintf(param$format.logical[posf,posi], di);    
      poss <- recycle(niv, nrow(param$style.logical));   
      posi <- recycle(colv, ncol(param$style.logical))[i];         
      ostyle[1+rowv,i+1] <- param$style.logical[poss,posi];
    } else if (typei=='character') {
      posf <- recycle(niv, nrow(param$format.char));         
      posi <- recycle(colv, ncol(param$format.char))[i];         
      output[1+rowv,i+1] <- sprintf(param$format.char[posf,posi], di);    
      poss <- recycle(niv, nrow(param$style.char));   
      posi <- recycle(colv, ncol(param$style.char))[i];     
      ostyle[1+rowv,i+1] <- param$style.char[poss,posi];     
    } else if ((typei=='double')||(typei=='integer')) {
      posf <- recycle(niv, nrow(param$format.numeric));         
      posi <- recycle(colv, ncol(param$format.numeric))[i];        
      output[1+rowv,i+1] <- sprintf(param$format.numeric[posf,posi], as.double(di));    
      poss <- recycle(niv, nrow(param$style.numeric));   
      posi <- recycle(colv, ncol(param$style.numeric))[i];      
      ostyle[1+rowv,i+1] <- param$style.numeric[poss,posi];
    } else stop(paste('unexpected data type "', typei, '"', sep=''));
  }
  ## build text
  if (is.null(type)) {
    type <- "rout not found";
    if (exists('rout', envir=.GlobalEnv)) type <- get('rout', envir=.GlobalEnv);
  }
  rows <- nrow(output);
  cols <- ncol(output);
  if (type=="html") {  
    btable   <-  sprintf('<table style="%s">', param$style.table);
    btr      <- "\n<tr>";
    btd      <- "\n<td style=\"%s\">";
    bcaption <- "\n<caption style=\"%s\">";
    ecaption <- "</caption>";
    etd      <- "</td>";
    etr      <- "</tr>";
    etable   <- "\n</table>\n";
  }
  else if (type=="wiki") {
    btable   <- sprintf('{| style="%s"', param$style.table);
    btr      <- "\n|-";
    btd      <- "\n| style=\"%s\" | ";
    bcaption <- "\n|+ style=\"%s\" | ";
    ecaption <- "";
    etd      <- "";
    etr      <- "";
    etable   <- "\n|}\n";
  }
  else { 
    btable   <- "\n";
    btr      <- "";
    btd      <- "";
    bcaption <- "";
    ecaption <- "\n";
    etd      <- "";
    etr      <- "\n";
    etable   <- "\n";
    maxlen   <- apply(nchar(output), 2, max);
    for (i in 1:cols) output[,i] <- sprintf("% *s", 1+maxlen[i], output[,i])      
    output[1,1] <- sprintf("%-*s", 1+maxlen[1], output[1,1])      
  }
  # build output text
  result <- btr;
  for (i in 1:cols) result <- paste(result, sprintf(btd, ostyle[,i]), output[,i], etd, sep='');
  result <- paste(paste(result, etr, sep=''), sep='', collapse="");
  result <- paste(btable,  if (nchar(param$style.caption)) paste(result, sprintf(bcaption, param$style.caption), param$caption, ecaption, sep='') else "", result, etable, sep='');
  result
}
