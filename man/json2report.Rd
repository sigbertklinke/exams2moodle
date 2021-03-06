% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/json2report.R
\name{json2report}
\alias{json2report}
\title{json2report}
\usage{
json2report(
  json,
  template = NULL,
  title = NULL,
  author = "",
  pdf_param = list(),
  ...
)
}
\arguments{
\item{json}{character: Name of the JSON file}

\item{template}{character: RMarkdown file with a program to create a report (default: \code{NULL})}

\item{title}{character: report title (default: \code{NULL})}

\item{author}{character: report author (default: \code{""})}

\item{pdf_param}{list: parameters given to \code{\link[rmarkdown:pdf_document]{rmarkdown::pdf_document()}} (default: \code{list()})}

\item{...}{further named parameter for replacements in the template file}
}
\value{
name of the generated pdf file
}
\description{
Creates a PDF report of the results and questions of JSON download of the detailled answers in Moodle.
A template file is used to create the report; if not given then the package default \code{hu_german.Rmd}
is used, see \code{system.file('template', 'hu_german.Rmd', package="exams2moodle")}.
If the \code{title} is not given then the file name of the JSON file will be used.
}
\details{
If your going to create your own templates then the following replacements are done
\describe{
\item{\code{\{\{author\}\}}}{is replaced by \code{author}}
\item{\code{\{\{title\}\}}}{is replaced by \code{title}}
\item{\code{\{\{json\}\}}}{is replaced by \code{json}}
}
Any futher named parameter will also replaced, e.g. \code{test="My test"} will lead to a replacement
in the template file \code{\{\{test\}\}} by \code{My test}.
}
\examples{
if (interactive()) {
  # copy JSON data file to current directory
  file.copy(system.file('json', 'Test-Klausur-Statistik.json', package="exams2moodle"), '.')
  # create a PDF report 'Test-Klausur-Statistik.pdf' including points and grades
  json2report('Test-Klausur-Statistik.json')
}
}
