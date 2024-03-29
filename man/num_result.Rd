% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/num_result.R
\name{num_result}
\alias{num_result}
\alias{int_result}
\title{num_result}
\usage{
num_result(x, digits = NULL, tolerance = NULL, tolmult = 2, ...)

int_result(x, ...)
}
\arguments{
\item{x}{numeric: rounded data}

\item{digits}{numeric: number of digits to rounding (default: \code{NULL})}

\item{tolerance}{numeric: tolerance for rounded data (default: \code{NULL})}

\item{tolmult}{numeric: multiplier for tolerance}

\item{...}{further parameters to \code{\link[exams:fmt]{exams::fmt()}}}
}
\value{
a list
}
\description{
\code{num_result} creates a list with the elements
}
\details{
\itemize{
\item \code{x} the original values
\item \code{fx} the rounded values with \code{\link[exams:fmt]{exams::fmt()}} as character
\item \code{tolerance} the tolerance
\item \code{digits} the digits used for rounding
}

Note that \code{x} may contain more than one numeric value to determine the rounding and tolerance.
Make sure that you use for numeric exercises \code{...$x[1]}.

If \code{digits} are not given and \code{length(x)>1} then \code{ceiling(-log10(min(diff(sort(x)), na.rm=TRUE)))} is used.
If \code{digits} are not given and \code{length(x)==1} then \code{3+ceiling(-log10(abs(x)))} is used.
If no \code{tolerance} is given then \code{tolmult*10^(1-digits)} is used.

\code{int_result} can be used if the result is an integer number and calls \code{num_result(x, 0, 0.1, 1, ...)} with
a tolerance of 0.1.
}
\examples{
# height for german man (in meter)
x <- rnorm(10, mean=1.8, sd =0.25)
num_result(c(mean(x), x), digits=2)
int_result(mean(x))
#
str(num_result(pi, 3))
str(num_result(pi, 6))
str(num_result(pi, 6, tolmult=5))
str(num_result(pi, 6, tolmult=5, tolerance=1e-6))
}
