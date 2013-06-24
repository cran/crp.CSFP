\name{integrity.check}
\docType{methods}
\alias{integrity.check}
\alias{integrity.check-methods}
\alias{integrity.check,character,crp.CSFP-method}
\title{Internal method to ensure model integrity}
\description{
This method is called each time you access a slot by \code{<slot>}. It checks the state of internal integrity flags in order to guarantee, that the slot you want to access has a value, consistent with the rest of the model.\cr
For example, if you run \code{crp.CSFP} (complete calculation of the model), then change \code{loss.unit} by \code{loss.unit(MyModel)<-...} to another value (which causes a call to \code{set.changes} ) and want to access \code{EL.crp}, a warning message is printed because you did not recalculate \code{EL.crp}. Instead you have to run \code{calc.portfolio.statistics}, \code{loss.dist} and \code{measure} again to get the right value for \code{EL.crp}.
}
\seealso{
\code{\link{crp.CSFP-class}},
\code{\link{crp.CSFP}},
}
\keyword{methods}
