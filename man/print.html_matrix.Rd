% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.html_matrix.R
\name{print.html_matrix}
\alias{print.html_matrix}
\title{print}
\usage{
\method{print}{html_matrix}(x, ..., which = "")
}
\arguments{
\item{x}{html_matrix object}

\item{...}{further parameters, unused}

\item{which}{character: which component to print (default: \code{""})}
}
\value{
invisible the text matrix shown
}
\description{
print a HTML matrix content or its components
}
\examples{
m <- matrix(1:6, ncol=2)
l <- html_matrix(m)
print(l, which=NA)      # returns full style information
print(l, which="fmt")   # returns format information
print(l, which="value") # identical to print(l)
}
