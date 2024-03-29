% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prob2.R
\name{data_prob2}
\alias{data_prob2}
\title{data_prob2}
\usage{
data_prob2(
  data = NULL,
  nrow = 2,
  ncol = 2,
  colnames = NULL,
  rownames = NULL,
  ...
)
}
\arguments{
\item{data}{an optional data vector. Non-atomic classed R objects are coerced
by \code{as.vector} and all attributes discarded.}

\item{nrow}{numeric: desired number of rows (default: \code{2})}

\item{ncol}{numeric: desired number of columns (default: \code{2})}

\item{colnames}{character: names of column events}

\item{rownames}{character: names of row events}

\item{...}{further parameters given to \code{\link[=ddiscrete]{ddiscrete()}}}
}
\value{
a matrix and some attributes
}
\description{
Generates a \code{nrow}x\code{ncol} matrix with probabilities/frequencies.
If \code{data} is given it will be normalized such that \code{sum(data[is.finite(data)])==1}.
If no \code{rownames} or \code{colnames} are given then event names from \code{LETTERS} are used.
The returned matrix will have the following attributes:
\itemize{
\item \code{marginals} a list of the row anc column marginal distributions,
\item \code{byrow} a matrix with conditional probabilities by row,
\item \code{bycol} a matrix with conditional probabilities by column,
\item \code{expected} a matrix with the expected probabilities under independence, and
\item \code{prob} a vector of all probabilities computed (except the expected ones).
}
}
\examples{
x <- data_prob2()
str(x)
data_prob2(colnames="E")
data_prob2(nrow=3)
}
