\name{read}
\docType{methods}
\alias{read}
\alias{crp.read}
\alias{crp.read,crp.CSFP-method}
\alias{read-methods}
\alias{read,crp.CSFP-method}
\title{Reading the input files}
\description{
This method reads the input files \code{port.name}, \code{rating.scale.name} and \code{sec.var.name} if needed (i.e. if \code{sec.var.est}=5) from the \code{path.in} directory and matches the data to the corresponding slots. Counterparties with \code{NEX=0}, \code{LGD=0}, \code{CP.rating=0} or \code{PD=0} are removed from the portfolio. Please make sure, that the input files have the correct form given in \code{\link{init}}.
In the context of the main routine \code{crp.CSFP} one can skip this method by setting \code{skip.read=TRUE}.

}
\seealso{
\code{\link{init}},
\code{\link{crp.CSFP}},
\code{\link{crp.CSFP-class}},
}
\keyword{methods}
