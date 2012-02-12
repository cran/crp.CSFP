\name{crp.calc.portfolio.statistics}
\docType{methods}
\alias{crp.calc.portfolio.statistics}
\alias{crp.calc.portfolio.statistics-methods}
\alias{crp.calc.portfolio.statistics,crp.CSFP-method}

\title{Calculating portfolio statistics}
\description{
This method calculates simple portfolio key numbers such as the expected loss (\code{EL}), standard deviation (\code{SD}), potential loss (\code{PL}) and others from the original data. Later it discretizes the losses according to the loss unit and adjusts the probabilities of default. Also the standard deviation for the sectors will be calculated from the \code{SD} of the rating classes or given sector variances. All information will be printed.
}
\seealso{
\code{\link{crp.CSFP-class}}
}
\keyword{methods}