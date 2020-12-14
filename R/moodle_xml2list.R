#' moodle_xml2list 
#'
#' Reads in a XML Moodle file generated with \code{exams2moodle} from the \code{exams} package.
#'
#' @param file character: XML file name
#' @param quiet logical: generate output during reading (default: \code{FALSE})
#'
#' @return a list with XML tags and bodys
#' @importFrom stringr str_sub str_detect str_match
#' @export
#'
#' @examples
#' # Read in a XML file generated from the test exercise from the exams package
#' file <- system.file('xml', 'rexams.xml', package="exams2moodle")
#' xml  <- moodle_xml2list(file, FALSE)
#' head(xml)
moodle_xml2list <- function(file, quiet=TRUE) {
  tagattr <- function(tag) {
    #    browser()
    taglist <- str_split(str_sub(tag, 2, nchar(tag)-1), "[\\s=]+")[[1]]
    ret <- taglist[1]
    if (length(taglist)>1) {
      i <- 2
      while(i<=length(taglist)) {
        if (startsWith(taglist[i], '"')) stop("Tag attribute without attribute name: ", tag)
        if (startsWith(taglist[i+1], '"')) {
          ret[taglist[i]] <- taglist[i+1]
          i <- i+2
        } else { 
          ret[taglist[i]] <- NA
          i <- 1+1
        }
      }
    }
    ret
  }
  #
  buildListArray <- function(la, key, tagdf, xml, quiet=TRUE) {
    n   <- 1
    repeat {
      if (!quiet) cat(paste0(rep(' ', length(key)), collapse=''), sprintf("%5.0f %s\n", nrow(tagdf), tagdf$tag[1])) 
      #      browser()
      tagpos <- (tagdf$type==tagdf$type[1]) - ((tagdf$type==paste0('/', tagdf$type[1])))
      endtag <- which(cumsum(tagpos)==0)
      if (length(endtag)==0) {
        print(head(tagdf))
        stop(sprintf("Tag pair not found: <%s>...</%s>", tagdf$type[1], tagdf$type[1]))
      }
      endtag <- endtag[1]
      #
      la[[makekey(c(key,n))]] <- tagattr(tagdf$tag[1])
      if (endtag==2) { # no children
        la[[makekey(c(key,n,0))]] <- str_sub(xml, tagdf$end[1]+1, tagdf$start[2]-1)
      } else { # children
        la <- buildListArray(la, c(key,n), tagdf[2:(endtag-1),], xml, quiet)
      }
      if (endtag>=nrow(tagdf)) break;
      #      browser()
      tagdf <- tagdf[(endtag+1):nrow(tagdf),]
      n <- n+1
    }
    la
  }
  #
  #browser()
  stopifnot(file.exists(file))
  xml  <- readChar(file, file.info(file)$size)
  res  <- str_locate_all(xml, '<.*?>')[[1]]
  tags <- str_sub(xml, res[,1], res[,2])
  validtag   <- str_detect(tags, "^<[/?a-zA_Z][a-zA-Z0-9]*" )
  htmltag    <- str_detect(tags, "^</*(table|thead|tbody|strong|code|span|img|pre|div|ul|ol|li|em|td|th|tr|a|p|i)[^a-zA-Z]") | str_detect(tags, '/>$')
  invalidtag <- which(!validtag | htmltag)
  if (length(invalidtag)) {
    res  <- res[-invalidtag,]
    tags <- tags[-invalidtag]
  }
  if (!quiet) {
    detectedtags <- str_match(tags, "^</*(.*?)[^a-zA-Z?]")
    tagtab <- table(detectedtags[,2])
    tago   <- order(names(tagtab))
    digits <- 1+max(log10(tagtab))
    cat("\nTags dectected:\n")
    cat(" ", sprintf("%*.0f %s\n", digits, tagtab[tago], names(tagtab)[tago]))
    cat("\n")
  }
  detectedtags <- str_match(tags, "^<(/*.*?)[^a-zA-Z?]")[,2]
  stopifnot(detectedtags[1]=='?xml')
  #  list(tag=tagattr(tags[1]), body=buildlist(data.frame(tag=tags[-1], res[-1,], type=detectedtags[-1]), xml, verbose))
  la    <- list()
  la[[makekey(1)]] <- tagattr(tags[1])
  buildListArray(la, 2, data.frame(tag=tags[-1], res[-1,], type=detectedtags[-1]), xml, quiet)
}