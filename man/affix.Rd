% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/affix.R
\name{affix}
\alias{affix}
\alias{math}
\alias{bracket}
\alias{unaffix}
\alias{unquote}
\alias{uncdata}
\alias{cdata}
\title{affix}
\usage{
affix(txt, prefix = "", suffix = "")

math(txt)

bracket(txt)

unaffix(txt, prefix = "", suffix = "")

unquote(txt)

uncdata(txt)

cdata(txt)
}
\arguments{
\item{txt}{vector: (character) vector to add a prefix and/or suffix}

\item{prefix}{character: prefix to add or delete (default: \code{""})}

\item{suffix}{character: suffix to add or delete (default: \code{""})}
}
\value{
character vector
}
\description{
\code{affix} adds a prefix and/or suffix to a (character) vector.

\code{math} adds a \code{$} as pre- and suffix to a (character) vector.

\code{bracket} adds a \code{(} as prefix and \code{)} as suffix to a (character) vector.

\code{unaffix} deletes a pre- and/or suffix to a (character) vector.

\code{unquote} deletes double quotes as prefix and suffix

\code{uncdata} deletes a \code{<![CDATA[} as prefix and \code{]]>} as suffix

\code{cdata} adds a \code{<![CDATA[} as prefix and \code{]]>} as suffix
}
\examples{
x <- runif(5)
affix(x, "$", "$")
math(x)
}
