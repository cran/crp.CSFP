\name{crp.CSFP.rc.vares}
\docType{methods}
\alias{crp.CSFP.rc.vares}
\alias{crp.CSFP.rc.vares,crp.CSFP-method}
\title{Calculating risk contributions to VaR and ES}
\description{
This method calculates the risk contributions on counterparty level to the value at risk (VaR), expected shortfall (ES) and the corresponding TAU. The confidence level that is taken is the last entry in the models alpha vector. It is necessary, that \code{crp.loss.CSFP} was run before, to compute and save required data.
}
\seealso{
\code{\link{crp.CSFP.loss}},
\code{\link{crp.CSFP.rc.sd}},
\code{\link{crp.export}},
\code{\link{crp.CSFP-class}},
\code{\link{crp.CSFP}},
}
\keyword{methods}
