#' @rdname affix
#' @title  affix
#' @description \code{affix} adds a prefix and/or suffix to a (character) vector.
#' 
#' @param txt vector: (character) vector to add a prefix and/or suffix 
#' @param prefix character: prefix to add or delete (default: \code{""})
#' @param suffix character: suffix to add or delete (default: \code{""})
#'
#' @return character vector
#' @export
#'
#' @examples
#' x <- runif(5)
#' affix(x, "$", "$")
#' math(x)
affix <- function(txt, prefix='', suffix='')  { 
  paste0(prefix, txt, suffix) 
}

#' @rdname affix
#' @description \code{math} adds a \code{$} as pre- and suffix to a (character) vector.
#' @export
math  <- function(txt) { 
  affix(txt, '$', '$') 
}

#' @rdname affix
#' @description \code{unaffix} deletes a pre- and/or suffix to a (character) vector.
#' @export
unaffix  <- function(txt, prefix='', suffix='')  { 
  index <- which(startsWith(txt, prefix))
  if (length(index)) txt[index] <- substring(txt[index], nchar(prefix)+1)
  index <- which(endsWith(txt, suffix))
  if (length(index)) txt[index] <- substring(txt[index], 1, nchar(txt)-nchar(suffix))
  txt
}

#' @rdname affix
#' @description \code{unquote} deletes double quotes as prefix and suffix
#' @export
unquote  <- function(txt) { unaffix(txt, '"', '"') }
  
#' @rdname affix
#' @description \code{uncdata} deletes a \code{<![CDATA[} as prefix and \code{]]>} as suffix
#' @export  
uncdata  <- function(txt) { unaffix(txt, '<![CDATA[', ']]>') }

#' @rdname affix
#' @description \code{cdata} adds a \code{<![CDATA[} as prefix and \code{]]>} as suffix
#' @export  
cdata  <- function(txt) { affix(txt, '<![CDATA[', ']]>') }