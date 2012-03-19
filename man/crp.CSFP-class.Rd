\name{crp.CSFP-class}
\docType{class}
\alias{crp.CSFP-class}

\title{Class crp.CSFP}
\description{
Modelling credit risks based on the concept of "CreditRisk+".
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{crp.init()}.
}
\section{Slots}{
  \describe{
    \item{\code{PATH.IN}:}{is a character string with the path to the directory, where are the input files. All input files have to be in this directory. It must end with ".\\\\".}
    \item{\code{PATH.OUT}:}{is a character string with the path to the directory, where the output should be written to, ending with ".\\\\".  The default for PATH.OUT is PATH.IN.
Be aware, that actually, by writing any output, the model will create a subdirectory in PATH.OUT with its name. So you can use your PATH.IN as PATH.OUT (which is the default case), create different models from the same input data (or different files, lying in the same directory PATH.IN), without worrying about the output path or overwriting other results.}
    \item{\code{PORT.NAME}:}{is a character string with the name of the portfolio file, ending with ".csv".}
    \item{\code{RISK.NAME}:}{is a character string with the name of the rating file, ending with ".csv".}
    \item{\code{PDVAR.NAME}:}{is a character string with the name of the file containing the sector variances, ending with ".csv".}
    \item{SEC.VAR.EST}{
is an indicator for the mode, the sector standard deviations should be calculated.\cr
1: \code{Sum of (weights * SD)}\cr
2: \code{[Sum of (weights * SD)] / MU(k)}\cr
3: \code{Sum of (sqrt(weights) * SD)}\cr
4: \code{[Sum of ((sqrt(weights) * SD)] / MU(k)}\cr
5: \code{Read variances from external file}\cr
with: \code{MU.(k)=sum(W[,k+1]*PD)}
}
    \item{\code{LOSS.UNIT}:}{is the discretisation parameter for net exposures.}
    \item{\code{NITER.MAX}:}{is the maximum number of exposure bands/probabilities being calculated. If a number smaller one is insert, the calculation stops at this level of the CDF. For this mode, an upper bound of \code{NITER.MAX.GLOBAL} is implemented to stop if this number of iterations is reached. If the desired confidence level is not reached till this threshold, a warning comes up. If you set \code{NITER.MAX} > \code{NITER.MAX.GLOBAL} manually, the threshold will be ignored.
}
    \item{\code{NITER.MAX.GLOBAL}:}{is the maximum number of iterations for \code{NITER.MAX} < 1. Be aware, that a high value can have high memory costs during the algorithm, even if the calculation of the CDF stops very much earlier. }
    \item{\code{ALPHA}:}{is the vector of confidence levels (between 0 and 1), the Value at risk and expected shortfall should be calculated. It should be no problem if the entries are not in an ascending order. For the risk contributions only the last entry will be considered. 
}
    \item{\code{PLOT.PDF}:}{is a flag for plotting the PDF or not. It will not be recognized if you start \code{crp.plot} directly.
}
    \item{\code{CALC.RISK.CONT}:}{is a flag for calculating the risk contributions or not. It will not be recognized if you start \code{crp.CSFP.rc.vares} or \code{crp.CSFP.rc.sd} directly.
}
    \item{\code{rating}:}{is a numeric vector with the different rating classes.}
    \item{\code{rating.PD}:}{is a numeric vector with the probabilities of default corresponding to the rating classes.}
    \item{\code{rating.SD}:}{is a numeric vector with the standard deviations of the probabilities of default corresponding to the rating classes. If you use \code{SEC.VAR.EST}=5 this slot is unused.}
    \item{\code{NS}:}{is a numeric value with the number of sectors.}
    \item{\code{NC}:}{is a numeric value with the number of counterparties.}
    \item{\code{SEC.VAR}:}{is a numeric vector with the sector variances from \code{PDVAR.NAME}.}
    \item{\code{CP.NR}:}{is a numeric vector with the counterparty numbers out of \code{PORT.NAME}.}
    \item{\code{CP.RATING}:}{is a numeric vector with the counterparty ratings out of \code{PORT.NAME}.}
    \item{\code{NEX}:}{is a numeric vector with the exposure of each counterparty out of \code{PORT.NAME}.}
    \item{\code{LGD}:}{is a numeric vector with the counterparty specific lgd, given in \code{PORT.NAME}.}
    \item{\code{PL0}:}{is a numeric vector with the potential loss for each counterparty before discretisation.}
    \item{\code{PD0}:}{is a numeric vector with the probability of default according to \code{CP.rating} for each counterparty before discretisation.}
    \item{\code{EL}:}{is a numeric value with the expected loss before discretisation.}
    \item{\code{EL.crp}:}{is a numeric value with the expected loss after discretisation, calculated from the PDF.}
    \item{\code{NU}:}{is a numeric vector with the discrete losses on counterparty level as multiples of \code{LOSS.UNIT}.}
    \item{\code{PL}:}{is a numeric vector with the potential loss for each counterparty adjusted to the discretisation.}
    \item{\code{PD}:}{is a numeric vector with the probability of default according to \code{CP.rating} for each counterparty adjusted to the discretisation.}
    \item{\code{M}:}{is a numeric value giving the maximal exposure band, which is calculated in \code{crp.CSFP.loss}. If \code{NITER.MAX} is an integer greater one and the CDF reached not 100 percent during the algorithm in \code{crp.CSFP.loss}, you get \code{M}=\code{NITER.MAX}, else \code{M} is the iteration step where the calculations stopped (at 100 percent if \code{NITER.MAX} is an integer or at the desired confidence level). }
    \item{\code{MU.K}:}{is a numeric vector with the average number of defaults per sector.MU.K=sum(W[,k+1]*PD)}
    \item{\code{V.K}:}{is a numeric vector with the average loss per sector. It is defined by V.K[k]=sum(W[,1+k]*PD*PL).}
    \item{\code{SIGMA.K}:}{is a numeric vector with the sector variances, calculated from \code{rating.SD} or just \code{SEC.VAR} according to \code{SEC.VAR.EST}.}
    \item{\code{SIGMA2_DIV}:}{is a numeric value with the diversifiable part of \code{SD}.}
    \item{\code{SIGMA2_SYST}:}{is a numeric value with the systemic part of \code{SD}.}
    \item{\code{SD}:}{is a numeric value with the portfolio standard deviation.}
    \item{\code{SD.crp}:}{is a numeric value with the portfolio standard deviation after discretisation, calculated from the PDF.}
    \item{\code{W}:}{is a matrix with the sector weights for each counterparty given in \code{PORT.NAME}.}
    \item{\code{ALPHA.MAX}:}{is a numeric value with the maximum value of the CDF.}
    \item{\code{a}:}{is a numeric value calculated in \code{crp.CSFP.loss}, necessary for the PDF and needed in \code{crp.CSFP.rc.vares}.}
    \item{\code{B}:}{is a matrix calculated in \code{crp.CSFP.loss}, necessary for the PDF and needed in \code{crp.CSFP.rc.vares}}
    \item{\code{PDF}:}{is a numeric vector with the probability density function.}
    \item{\code{CDF}:}{is a numeric vector with the cumulative distribution function.}
    \item{\code{LOSS}:}{is a numeric vector with the different losses, the PDF is calculated for.}
    \item{\code{PLOT.SCALE}:}{is a numeric value defining the scale for the horizontal axis (the losses) of the plot of the PDF.}
    \item{\code{PLOT.RANGE.X}:}{is a numeric vector with two entries representing the range on the x-axis (the losses) for the plot of the loss distribution. If you insert values smaller one, this will be interpreted as levels of the CDF. The defaults for PLOT.RANGE.X/-Y (0,0) have the meaning, that R will choose axis range by itself.
}
    \item{\code{PLOT.RANGE.Y}:}{is the same as \code{PLOT.RANGE.X} above for vertical axis.}
    \item{\code{VaR}:}{is a numeric vector containing the calculated value at risk for the given levels in \code{ALPHA}.}
    \item{\code{EC}:}{is a numeric vector containing the economic capital for the given levels in \code{ALPHA}. In the CR+ framework this is defined as \code{EC=VaR-EL.crp}.}
    \item{\code{ES}:}{is a numeric vector containing the expected shortfall for the given levels in \code{ALPHA}.}
    \item{\code{VaR.CONT}:}{is a numeric vector with the risk contributions to the last entry in \code{VaR} for each counterparty.}
    \item{\code{ES.CONT}:}{is a numeric vector with the risk contributions to the last entry in \code{ES} for each counterparty.}
    \item{\code{ES.TAU.CONT}:}{is a numeric vector with the risk contributions to TAU, corresponding to the the last entry in \code{ES} for each counterparty.}
    \item{\code{SD.CONT}:}{is a numeric vector with the risk contributions to the portfolio standard deviation.}
    \item{\code{read.OK}:}{is a flag indicating if reading input files \code{PORT.NAME}, \code{RISK.NAME} and \code{PDVAR.NAME} was successful.}
    \item{\code{plausi.OK}:}{is a flag indicating if the plausibility check was successful.}
    \item{\code{rc.OK}:}{is a flag indicating if the calculation of risk contributions to VaR and ES was successful.}
    \item{\code{save.memory}:}{is a switch for the save memory mode. If \code{save.memory}=TRUE, \code{LOSS} and \code{CDF} will not be stored permanently. Instead they are erased after exporting the loss distribution. If needed, the CDF is recalculated from the PDF. The same happens to \code{a} and \code{B} at the end of \code{crp.CSFP.loss} if \code{CALC.RISK.CONT}=FALSE. If \code{save.memory} is FALSE all data will be kept.}
    \item{\code{SELV}:}{is a numeric vector with the positions of \code{VaR} in \code{CDF}.}
    \item{\code{ALPHA.crp}:}{is a numeric vector containing the CDF-levels of \code{VaR}. Because they are from the calculated CDF \code{ALPHA.crp} will always a little bit greater as \code{ALPHA}. With the help of \code{ALPHA.crp} it is not necessary to store the CDF for calculating risk contributions to VaR and ES.}
    \item{\code{NAME}:}{is a character string with the name of the model. This slot is set at the time you run the first method on your model, not by creation via \code{crp.init}. If you change the model name (the name of the R object) \code{NAME} is updated automatically by the first method running on it.}
    \item{\code{changes.crp.read}:}{is an internal flag indicating changes on input parameters (by call to \code{set.<slot>}) affecting \code{crp.read}.}
    \item{\code{changes.crp.plausi}:}{is an internal flag indicating changes on input parameters (by call to \code{set.<slot>}) affecting \code{crp.plausi}.}
    \item{\code{changes.crp.calc.portfolio.statistics}:}{is an internal flag indicating changes on input parameters (by call to \code{set.<slot>}) affecting \code{crp.calc.portfolio.statistics}.}
    \item{\code{changes.crp.CSFP.loss}:}{is an internal flag indicating changes on input parameters (by call to \code{set.<slot>}) affecting \code{crp.CSFP.loss}.}
    \item{\code{changes.crp.measure}:}{is an internal flag indicating changes on input parameters (by call to \code{set.<slot>}) affecting \code{crp.measure}.}
    \item{\code{changes.crp.plot}:}{is an internal flag indicating changes on input parameters (by call to \code{set.<slot>}) affecting \code{crp.plot}.}
    \item{\code{changes.crp.CSFP.rc.vares}:}{is an internal flag indicating changes on input parameters (by call to \code{set.<slot>}) affecting \code{crp.CSFP.rc.sd}.}
    \item{\code{changes.crp.CSFP.rc.sd}:}{is an internal flag indicating changes on input parameters (by call to \code{set.<slot>}) affecting \code{crp.CSFP.rc.vares}.}
    \item{\code{changes.crp.export}:}{is an internal flag indicating changes on input parameters (by call to \code{set.<slot>}) affecting \code{crp.export}.}
    \item{\code{file.format}:}{is a character string defining the format of the input files. You can choose between 'csv', which means that the separation character is ',' and the decimal character is '.' and 'csv2', which means that the separation character is ';' and the decimal character is ','.}
    \item{\code{input}:}{is a temporary list, used if inupt data (portfolio, risk.matrix, sec.var) are passed directly to \code{crp.init}.}
}
}
\section{Methods}{
  \describe{
    \item{crp.calc.portfolio.statistics}{Calculating portfolio statistics}
    \item{crp.CSFP.loss}{Calculating the loss distribution}
    \item{crp.CSFP.rc.sd}{Calculating risk contributions to standard deviation}
    \item{crp.CSFP.rc.vares}{Calculating risk contributions to VaR and ES}
    \item{crp.CSFP}{Main routine for CSFP-model}
    \item{crp.export}{Export risk contributions}
    \item{crp.integrity.check}{Internal method to ensure model integrity}
    \item{crp.measure}{Calculating portfolio measures}
    \item{crp.plausi}{Checking input data for plausibility}
    \item{crp.plot}{Plotting the PDF}
    \item{crp.read}{Reading the input files}
    \item{crp.set.changes}{Internal method for model integrity}
    \item{crp.summary}{Summarize portfolio key numbers}
    \item{crp.write.summary}{Writing summary to file}
    \item{get.<slot>}{You can enter every slot via \code{get.<slot>} e.g. \code{get.VaR(MyModel)}}
    \item{set.<slot> <-}{You can change the value of a slot iff this is an input parameter via \code{get.<slot>} e.g. \code{set.ALPHA(MyModel)<-c(0.999,0.9995)}. You can do so with all slots that are available in \code{crp.init}. }
	 }
}
\references{
First Boston Financial Products, "CreditRisk+", 1997 \cr
Gundlach & Lehrbass, "CreditRisk+ in the Banking Industry", Springer, 2003
}
\seealso{
\code{\link{crp.init}}
\code{\link{crp.CSFP}}
}
\author{
Dr. Matthias Fischer, Kevin Jakob & Stefan Kolb
}
\examples{
MyModel=crp.init(PATH.IN=paste(system.file(package="crp.CSFP"),"/data/",sep=""),LOSS.UNIT=1e6,CALC.RISK.CONT=TRUE)

# or pass portfolio directly and use random sector variances
Path=paste(system.file(package="crp.CSFP"),"/data/",sep="")
portfolio=read.csv(paste(Path,"portfolio.csv",sep=""))
risk.matrix=read.csv(paste(Path,"rating_pd.csv",sep=""))
sec.var=data.frame(Var=runif(3,0,2))

MyModel=crp.init(portfolio=portfolio,risk.matrix=risk.matrix,sec.var=sec.var,LOSS.UNIT=1e6,CALC.RISK.CONT=TRUE,PATH.OUT=Path)

# execute portfolio calculation
MyModel=crp.CSFP(MyModel)
}
\keyword{classes}
