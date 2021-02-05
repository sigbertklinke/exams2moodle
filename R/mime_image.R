#' mime_image
#'
#' Returns the MIME type of an image based on the filename extension. 
#' If a MIME type for a file extension is not found then the extension itself will be returned.
#'
#' @param filename character: file name
#'
#' @return character
#' @importFrom stringr str_match_all
#' @export
#'
#' @examples
#' mime_image("support.png")
#' mime_image("support.jpg")
mime_image <- function(filename) {
  fext  <- str_match_all(basename(filename), "\\..*?$")[[1]]
  stopifnot(length(fext)>0)
  # from https://www.lifewire.com/file-extensions-and-mime-types-3469109
  if (fext==".jpg") return("jpeg")    
  if (fext==".jpe") return("jpeg")  
  if (fext==".svg") return("svg+xml")
  if (fext==".tif") return("tiff")
  if (fext==".ico") return("x-icon")
  if (fext==".pnm") return("x-portable-anymap") 
  if (fext==".pbm") return("x-portable-bitmap") 
  if (fext==".pgm") return("x-portable-graymap") 
  if (fext==".ppm") return("x-portable-pixmap") 
  if (fext==".rgb") return("x-rgb") 
  if (fext==".xbm") return("x-xbitmap") 
  if (fext==".xpm") return("x-xpixmap") 
  if (fext==".xwd") return("x-xwindowdump") 
  #
  if (fext==".cod") return("cis-cod")
  if (fext==".jfif") return("pipeg")
  if (fext==".ras") return("x-cmu-raster")
  if (fext==".cmx") return("x-cmx")
  #
  return(substring(fext, 2))
}