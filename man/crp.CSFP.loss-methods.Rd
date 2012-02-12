\name{crp.CSFP.loss}
\docType{methods}
\alias{crp.CSFP.loss}
\alias{crp.CSFP.loss,crp.CSFP-method}

\title{Calculating the loss distribution}
\description{
This method uses an algorithm from Gundlach/Lehrbass p.74f to compute the loss distribution. A small modification on the structure of the two loops was done to compute the CDF parallel to the PDF and stop if a desired level is reached. 
Data \code{a} and \code{B} (corresponding to alpha and b in Gundlach/Lehrbass p.74), necessary for risk contributions, will be erased at the end if \code{CALC.RISK.CONT}=FALSE and \code{save.memory}=TRUE.
}
\seealso{
\code{\link{crp.CSFP.rc.vares}},
\code{\link{crp.CSFP-class}},
\code{\link{crp.CSFP}},
}
\keyword{methods}
