\name{calc.portfolio.statistics}
\docType{methods}
\alias{calc.portfolio.statistics}
\alias{crp.calc.portfolio.statistics}
\alias{crp.calc.portfolio.statistics,crp.CSFP-method}
\alias{calc.portfolio.statistics-methods}
\alias{calc.portfolio.statistics,crp.CSFP-method}
\title{Calculating portfolio statistics}
\description{
This method calculates simple portfolio key numbers such as the expected loss (\code{EL}), standard deviation (\code{SD}), potential loss (\code{PL}) and others from the original data. Later losses are discretized according to the loss unit and probabilities of default are adjusted. Also the standard deviation for the sectors will be calculated from the \code{SD} of the rating classes or given sector variances. Counterparties with potential loss equal to zero or PD of zero are removed for further calculations. All information will be printed.
}
\seealso{
\code{\link{crp.CSFP-class}}
}
\keyword{methods}
