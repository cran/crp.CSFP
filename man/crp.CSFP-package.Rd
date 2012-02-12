\name{crp.CSFP-package}
\alias{crp.CSFP-package}
\docType{package}
\title{
CreditRisk+ portfolio model
}
\description{
Modelling credit risks based on the concept of "CreditRisk+", First Boston Financial Products, 1997 and "CreditRisk+ in the Banking Industry", Gundlach & Lehrbass, Springer, 2003.
}
\details{The package provides the opportunity to analyze a given credit portfolio on a very simple level. Key numbers, that can be calculated are the expected loss, standard deviation, value at risk and expected shortfall on any confidence level, as well as risk contributions to them on counterparty level.
 The results (i.e. the loss distribution) are achieved by an analytical approach. Therefore a lot of theoretical assumptions are necessary. So please make yourself familiar with the framework of this model given in "CreditRisk+", First Boston Financial Products, 1997, before using it.
 
\tabular{ll}{
Package: \tab crp.CSFP\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-02-12\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
Depends: \tab methods\cr
}
For first use have a look at \code{\link{crp.CSFP-class}}, \code{\link{crp.CSFP}} or \code{\link{crp.init}}
}
\author{
Dr. Matthias Fischer, Kevin Jakob & Stefan Kolb

Maintainer: Dr. Matthias Fischer <Matthias.Fischer@wiso.uni-erlangen.de>
}
\references{
First Boston Financial Products, "CreditRisk+", 1997 \cr
Gundlach & Lehrbass, "CreditRisk+ in the Banking Industry", Springer, 2003
}
\keyword{ package }
\seealso{
\code{\link{crp.CSFP-class}}
\code{\link{crp.CSFP}}
\code{\link{crp.init}}
}