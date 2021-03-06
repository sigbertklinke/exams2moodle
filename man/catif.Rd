% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catif.R
\name{catif}
\alias{catif}
\title{catif}
\usage{
catif(cond, ...)
}
\arguments{
\item{cond}{logical: condition, if true then \code{cat} is called otherwise not}

\item{...}{further parameter given to \code{cat}}
}
\value{
nothing
}
\description{
Calls \code{cat} if \code{cond==TRUE}.
}
\examples{
catif(TRUE, "PDF")      # should appear
catif(FALSE, "Moodle")  # should not appear
}
