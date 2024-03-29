% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mime_image.R
\name{mime_image}
\alias{mime_image}
\title{mime_image}
\usage{
mime_image(filename)
}
\arguments{
\item{filename}{character: file name}
}
\value{
character
}
\description{
Returns the MIME type of an image based on the filename extension.
If a MIME type for a file extension is not found then the extension itself will be returned.
}
\examples{
mime_image("support.png")
mime_image("support.jpg")
}
