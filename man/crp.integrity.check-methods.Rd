\name{crp.integrity.check}
\docType{methods}
\alias{crp.integrity.check}
\alias{crp.integrity.check,character,crp.CSFP-method}
\title{Internal method to ensure model integrity}
\description{
This method is called each time you access a slot by \code{get.<slot>}. It checks the state of internal integrity flags in order to guarantee, that the slot you want to access has a value, consistent with the rest of the model.\cr
For example, if you run \code{crp.CSFP} (complete calculation of the model), then change \code{LOSS.UNIT} by \code{set.LOSS.UNIT<-} to another value (which causes a call to \code{crp.set.changes} ) and want to access \code{EL.crp}, a warning message is printed because you did not recalculate \code{EL.crp}. Instead you have to run \code{crp.calc.portfolio.statistics}, \code{crp.CSFP.loss} and \code{crp.measure} again to get the right value for \code{EL.crp}.
}
\seealso{
\code{\link{crp.CSFP-class}},
\code{\link{crp.CSFP}},
}
\keyword{methods}
