\name{crp.read}
\docType{methods}
\alias{crp.read}
\alias{crp.read,crp.CSFP-method}
\title{Reading the input files}
\description{
This method reads the input files \code{PORT.NAME}, \code{RISK.NAME} and \code{PDVAR.NAME} if needed (i.e. if SEC.VAR.EST=5) from the \code{PATH.IN} directory and matches the data to the corresponding slots. Counterparties with \code{NEX=0}, \code{LGD=0}, \code{CP.RATING=0} or \code{PD0=0} are removed from the portfolio. Please make sure, that the input files have the correct form given in \code{\link{crp.init}}.

}
\seealso{
\code{\link{crp.init}},
\code{\link{crp.CSFP}},
\code{\link{crp.CSFP-class}},
}
\keyword{methods}
