\name{crp.plausi}
\docType{methods}
\alias{crp.plausi}
\alias{crp.plausi,crp.CSFP-method}
\title{Checking input data for plausibility}
\description{
This method checks the input data for plausibility. The following checks are done:
In \code{RISK.MATRIX}: The PD should be non-decreasing according to the RATING.\cr
In \code{PORT.NAME}: The exposure should not be negative.\cr
In \code{PORT.NAME}: The sector weights of each CP should not be negative and sum up to a number less or equal 1.\cr
In \code{PDVAR.NAME}: The variances should not be negative.\cr  
}
\seealso{
\code{\link{crp.CSFP}},
\code{\link{crp.CSFP-class}},
}
\keyword{methods}