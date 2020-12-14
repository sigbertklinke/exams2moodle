#' json2report
#'
#' Creates a PDF report of the results and questions of JSON download of the detailled answers in Moodle.
#' A template file is used to create the report; if not given then the package default \code{hu_german.Rmd}
#' is used, see \code{system.file('template', 'hu_german.Rmd', package="exams2moodle")}.
#' If the \code{title} is not given then the file name of the JSON file will be used.
#'
#' @param json character: Name of the JSON file
#' @param template character: RMarkdown file with a program to create a report (default: \code{NULL})
#' @param title character: report title (default: \code{NULL})
#' @param author character: report author (default: \code{""})
#' @param pdf_param list: parameters given to [rmarkdown::pdf_document()] (default: \code{list()})
#' @param ... further named parameter for replacements in the template file 
#'
#' @details If your going to create your own templates then the following replacements are done
#' \describe{
#' \item{\code{\{\{author\}\}}}{is replaced by \code{author}}
#' \item{\code{\{\{title\}\}}}{is replaced by \code{title}}
#' \item{\code{\{\{json\}\}}}{is replaced by \code{json}}
#' } 
#' Any futher named parameter will also replaced, e.g. \code{test="My test"} will lead to a replacement 
#' in the template file \code{\{\{test\}\}} by \code{My test}.
#'
#' @return name of the generated pdf file
#' @importFrom rmarkdown render pdf_document
#' @export
#' @md
#'
#' @examples
#' if (interactive()) {
#'   # copy JSON data file to current directory
#'   file.copy(system.file('json', 'Test-Klausur-Statistik.json', package="exams2moodle"), '.')
#'   # create a PDF report 'Test-Klausur-Statistik.pdf' including points and grades
#'   json2report('Test-Klausur-Statistik.json')
#' }
json2report <- function(json, template=NULL, title=NULL, author='', pdf_param=list(), ...) {
  # files
  dir   <- dirname(json)
  if (nchar(dir)==0) dir <- "."
  file  <- basename(json)
  fname <- gsub("\\..*?$", "", file)
  ext   <- substring(file, nchar(fname)+2)
  #
  if (is.null(title)) title <- fname
  if (is.null(template)) template <- system.file('template', 'hu_german.Rmd', package="exams2moodle") 
  #  
  tcont <- readLines(template)
  treplace <- list(...)
  treplace$author <- author
  treplace$title  <- title
  treplace$json   <- json
  for (name in names(treplace)) tcont <- gsub(affix(name, '{{', '}}'), treplace[[name]], tcont, fixed=TRUE)
  rmd   <- paste0(dir, '/', fname, '.Rmd')
  writeLines(tcont, rmd)
  #browser()
  pdf   <- paste0(dir, '/', fname, '.pdf')
  render(rmd, output_file=pdf, do.call(pdf_document, pdf_param))
  pdf
}