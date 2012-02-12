\name{crp.set.changes}
\docType{methods}
\alias{crp.set.changes}
\alias{crp.set.changes,character,crp.CSFP-method}
\title{Internal method for model integrity}
\description{
The method is called by \code{crp.set.changes} every time an input parameter is changed by an \code{set.<slot>} method in order to set flags indicating if the integrity of the model is satisfied or at witch point in the algorithms changes come into effect.
}
\seealso{
\code{\link{crp.integrity.check}}
\code{\link{crp.CSFP}}
\code{\link{crp.CSFP-class}}
}
\keyword{methods}
