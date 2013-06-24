\name{crp.CSFP}
\alias{crp.CSFP}
\alias{crp.CSFP,crp.CSFP-method}
\alias{crp.CSFP-methods}
\alias{crp.CSFP,crp.CSFP,logical-method}
\alias{crp.CSFP,crp.CSFP,missing-method}
\title{Main routine for CSFP-model}
\description{
This is a summary of all methods you need during a complete run of the model (see details). 
}
\usage{
crp.CSFP(this, skip.read)
}
\arguments{
  \item{this}{
object of class crp.CSFP
}
  \item{skip.read}{
logical, indicating if the \code{read} function should be executed. I.e. it defines is input data should be read from input path. Set this value to \code{FALSE} if the model is already fully initialized.
If portfolio data are manipulated manually, for example by \code{NEX(MyModel)=...} and recalculation should be performed, \code{skip.read} should be set to \code{FALSE}.
}
}
\details{
It executes the following methods in the given order.\cr
\code{read} (if \code{skip.read} = \code{TRUE}, which is default) \cr
\code{plausi}\cr
\code{calc.portfolio.statistics}\cr
\code{loss.dist}\cr
\code{measure}\cr
\code{plot}\cr
\code{rc.vares}\cr
\code{rc.sd}\cr
\code{export}\cr
If errors occur at \code{read} or \code{plausi} it stops. The state of \code{plot.PDF} and \code{calc.rc} are respected. During the computation, \code{save.memory} will be set to FALSE because of performance considerations. At the end, it will be switched back to the original state and all aftermath come into effect.
}
\value{
Object of class crp.CSFP
}
\seealso{
\code{\link{read}},
\code{\link{plausi}},
\code{\link{calc.portfolio.statistics}},
\code{\link{loss.dist}},
\code{\link{measure}},
\code{\link{rc.vares}},
\code{\link{rc.sd}},
\code{\link{export}},
\code{\link{crp.CSFP-class}},
}
\keyword{methods}
