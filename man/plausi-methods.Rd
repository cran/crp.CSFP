\name{plausi}
\docType{methods}
\alias{plausi}
\alias{crp.plausi}
\alias{crp.plausi,crp.CSFP-method}
\alias{plausi-methods}
\alias{plausi,crp.CSFP-method}
\title{Checking input data for plausibility}
\description{
This method checks the input data for plausibility. The following checks are done:
The PD for ratign classes should be non-decreasing according to the rating.\cr
The exposure should not be negative.\cr
The sector weights of each CP should not be negative and sum up to a number less or equal 1.\cr
The sector variances should not be negative.\cr  
}
\seealso{
\code{\link{crp.CSFP}},
\code{\link{crp.CSFP-class}},
}
\keyword{methods}
