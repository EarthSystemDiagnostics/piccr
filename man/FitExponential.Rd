% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LibFunctions.R
\name{FitExponential}
\alias{FitExponential}
\title{Fit exponential}
\usage{
FitExponential(inj_no, mem.coeff)
}
\arguments{
\item{inj_no}{numeric vector of injection numbers corresponding to the
memory coefficients in \code{mem.coeff}.}

\item{mem.coeff}{numeric vector of estimated memory coefficients to fit.}
}
\value{
a list with the two fit parameters \code{a} and \code{b}; see
details.
}
\description{
Fit an exponential decay function to memory coefficients.
}
\details{
The following exponential fit is applied:
\code{mem.coeff ~ 1 - a * exp(-b * inj.no)}.
}
\author{
Thomas Münch
}
