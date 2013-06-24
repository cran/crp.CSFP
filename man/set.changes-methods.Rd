\name{set.changes}
\docType{methods}
\alias{set.changes}
\alias{crp.set.changes}
\alias{crp.set.changes,character,crp.CSFP-method}
\alias{set.changes-methods,character,crp.CSFP-method}
\alias{set.changes,character,crp.CSFP-method}
\title{Internal method for model integrity}
\description{
The method is called by \code{set.changes} every time an input parameter is changed by an \code{<slot>} method in order to set flags indicating if the integrity of the model is satisfied or at witch point in the algorithms changes come into effect.
}
\seealso{
\code{\link{integrity.check}}
\code{\link{crp.CSFP}}
\code{\link{crp.CSFP-class}}
}
\keyword{methods}
