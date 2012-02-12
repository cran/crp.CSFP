\name{crp.CSFP}
\docType{methods}
\alias{crp.CSFP}
\alias{crp.CSFP,crp.CSFP-method}
\title{Main routine for CSFP-model}
\description{
This is a summary of all methods you need during a complete run of the model. It executes the following methods in the given order.\cr
\code{crp.read}\cr
\code{crp.plausi}\cr
\code{crp.calc.portfolio.statistics}\cr
\code{crp.CSFP.loss}\cr
\code{crp.measure}\cr
\code{crp.plot}\cr
\code{crp.CSFP.rc.vares}\cr
\code{crp.CSFP.rc.sd}\cr
\code{crp.export}\cr
If errors occur at \code{crp.read} or \code{crp.plausi} it stops. The state of \code{PLOT.PDF} and \code{CALC.RISK.CONT} will be respected. During the computation, \code{save.memory} will be set to FALSE because of performance considerations. At the end, it will be switched back to the original state and all aftermath come into effect.
}
\seealso{
\code{\link{crp.read}},
\code{\link{crp.plausi}},
\code{\link{crp.calc.portfolio.statistics}},
\code{\link{crp.CSFP.loss}},
\code{\link{crp.measure}},
\code{\link{crp.CSFP.rc.vares}},
\code{\link{crp.CSFP.rc.sd}},
\code{\link{crp.export}},
\code{\link{crp.CSFP-class}},
}
\keyword{methods}