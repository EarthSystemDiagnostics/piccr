% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LibFunctions.R
\name{FinalDataFormat}
\alias{FinalDataFormat}
\title{Data output format}
\usage{
FinalDataFormat(x)
}
\arguments{
\item{x}{a numeric vector of Picarro data for a given measurement injection
sequence.}
}
\value{
the isotopic data from \code{x}.
}
\description{
Format isotope data for text file output, i.e. strip the line and injection
numbers from the data and retain only the isotope data complemented by the
d-excess value.
}
\author{
Thomas Münch
}
