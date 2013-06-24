\name{export}
\docType{methods}
\alias{export}
\alias{crp.export}
\alias{crp.export,crp.CSFP-method}
\alias{export-methods}
\alias{export,crp.CSFP-method}
\title{Export risk contributions and loss distribution}
\description{
This method exports the risk contributions, calculated via \code{rc.vares} and \code{rc.sd} the loss distribution and a summary of the model to files named "RC.csv", "lossdist.csv" and "summary.csv" in the \code{path.out/name} directory. In \code{save.memory} mode, the content of the risk contribution attributes will be deleted afterwards.
}
\seealso{
\code{\link{rc.vares}},
\code{\link{rc.sd}},
\code{\link{crp.CSFP}},
\code{\link{crp.CSFP-class}},
}
\keyword{methods}
