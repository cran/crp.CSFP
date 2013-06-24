\name{crp.CSFP-class}
\Rdversion{1.1}
\docType{class}
\alias{crp.CSFP-class}
\title{Class \code{"crp.CSFP"}}
\description{
Modelling credit risks based on the concept of "Credit Risk+".}
\section{Objects from the Class}{
Objects can be created by calls of the form MyModel=\code{init(...)}.
}
\section{Slots}{
  \describe{
    \bold{portfolio data:}\cr
    \item{\code{CP.NR}:}{is a numeric vector with the counterparty ID-numbers.}
    \item{\code{CP.rating}:}{is a numeric vector with the counterparty ratings.}
    \item{\code{NEX}:}{is a numeric vector with the exposure of each counterparty.}
    \item{\code{LGD}:}{is a numeric vector with the counterparty specific LGDs}
    \item{\code{PL}:}{is a numeric vector with the potential loss for each counterparty before discretization.}
    \item{\code{PD}:}{is a numeric vector with the probability of default according to \code{CP.rating} for each counterparty     before discretization.}
    \item{\code{W}:}{is a matrix with the sector weights for each counterparty.}
    \item{\code{nu}:}{is a numeric vector with the discrete losses on counterparty level as multiples of \code{loss.unit}.}
    \item{\code{PL.crp}:}{is a numeric vector with the potential loss for each counterparty adjusted to the discretization.}
    \item{\code{PD.crp}:}{is a numeric vector with the probability of default according to \code{CP.rating} for each counterparty adjusted to the discretization.}
    
    
    \bold{rating information:}\cr
    \item{\code{rating}:}{is a numeric vector with the different rating classes.}
    \item{\code{rating.PD}:}{is a numeric vector with the probabilities of default corresponding to the rating classes.}
    \item{\code{rating.SD}:}{is a numeric vector with the standard deviations of the probabilities of default corresponding to the rating classes. If you use \code{sec.var.est}=5 this slot is unused.}
    
    
    \bold{sector information:}\cr
    \item{\code{mu.k}:}{is a numeric vector with the average number of defaults per sector \code{mu.k=sum(W[,k+1]*PD.crp)}}
    \item{\code{loss.k}:}{is a numeric vector with the average loss per sector. It is defined by \cr \code{loss.K[k]=sum(W[,1+k]*PD*PL)}.}
    \item{\code{sigma.k}:}{is a numeric vector with the sector standard deviation, calculated from \code{rating.SD} or \code{sec.var} according to \code{sec.var.est}.}

     
  \bold{control parameters:}\cr
    \item{\code{sec.var.est}:}{is an indicator for the mode, the sector standard deviations should be calculated.\cr
1: \code{Sum of (weights * SD)}\cr
2: \code{[Sum of (weights * SD)] / MU(k)}\cr
3: \code{Sum of (sqrt(weights) * SD)}\cr
4: \code{[Sum of ((sqrt(weights) * SD)] / MU(k)}\cr
5: \code{Read variances from external file}\cr
with: \code{MU.(k)=sum(W[,k+1]*PD)}}
    \item{\code{loss.unit}:}{is the discretization parameter for net exposures.}
    \item{\code{Niter.max}:}{is the maximum number of exposure bands/probabilities being calculated.}
    \item{\code{alpha.max}:}{If a number smaller one is insert, the calculation stops at this level of the CDF. For this mode, an upper bound of \code{Niter.max.global} is implemented to stop if this number of iterations is reached. If the desired confidence level is not reached till this threshold, a warning comes up. If you set \code{Niter.max} > \code{Niter.max.global} manually, the threshold will be ignored.}
    \item{\code{Niter.max.global}:}{is the maximum number of iterations if \code{alpha.max} is specified. Be aware, that a high value can have high memory costs during the algorithm, even if the calculation of the CDF stops very much earlier. }
    \item{\code{alpha}:}{is the vector of confidence levels (between 0 and 1), the Value at risk and expected shortfall should be calculated. It should be no problem if the entries are not in an ascending order. For the risk contributions only the last entry will be considered.}
    \item{\code{PLOT.PDF}:}{is a flag for plotting the PDF during \code{crp.CSFP} main routine or not. It will not be recognized if you start \code{plot} directly.}
    \item{\code{PLOT.scale}:}{is a numeric value defining the scale for the horizontal axis (the losses) of the PDF plot.}
    \item{\code{PLOT.range.x}:}{is a numeric vector with two entries representing the range on the x-axis (the losses) for the plot of the loss distribution. If you insert values smaller one, this will be interpreted as levels of the CDF. The default for \code{PLOT.range.x /-.y} (0,0) means, that R will choose axis range by itself.}
    \item{\code{PLOT.range.y}:}{is the same as \code{PLOT.range.x} above for vertical axis.}
    \item{\code{calc.rc}:}{is a flag for calculating the risk contributions or not during \code{crp.CSFP} main routine. It will not be recognized if you start \code{rc.vares} or \code{rc.sd} directly.}
    \item{\code{save.memory}:}{is a switch for the save memory mode. If \code{save.memory}=TRUE, \code{loss} and \code{CDF} will not be stored permanently. If needed, the CDF is recalculated from the PDF. The same happens to \code{a} and \code{B} at the end of \code{loss.dist} if \code{calc.rc}=FALSE.}

    
    \bold{model information and risk measures:}\cr
    \item{\code{NS}:}{is a numeric value with the number of sectors.}
    \item{\code{NC}:}{is a numeric value with the number of counterparties.}
    \item{\code{sec.var}:}{is a numeric vector with the sector variances from \code{sec.var.name}.}
    \item{\code{EL}:}{is a numeric value with the expected loss before discretization.}
    \item{\code{EL.crp}:}{is a numeric value with the expected loss after discretization, calculated from the PDF. Differences to \code{EL} are caused by \code{alpha.max} or \code{Niter.max} being to small.}
    \item{\code{sigma.sqr.div}:}{is a numeric value with the diversifiable part of \code{SD.crp}. The value is already squared.}
    \item{\code{sigma.sqr.syst}:}{is a numeric value with the systemic part of \code{SD}. The value is already squared.}
    \item{\code{SD}:}{is a numeric value with the portfolio standard deviation.}
    \item{\code{SD.crp}:}{is a numeric value with the portfolio standard deviation after discretization, calculated from the PDF. Differences to \code{SD} are caused by \code{loss.unit} being to large or \code{alpha.max} being to small.}
    \item{\code{VaR}:}{is a numeric vector containing the calculated value at risk for the given levels in \code{alpha}.}
    \item{\code{EC}:}{is a numeric vector containing the economic capital for the given levels in \code{alpha}. In the CR+ framework this is defined as \code{EC=VaR-EL.crp}.}
    \item{\code{ES}:}{is a numeric vector containing the expected shortfall for the given levels in \code{alpha}.}

    \bold{risk contributions:}\cr
    \item{\code{VaR.cont}:}{is a numeric vector with the risk contributions to the last entry in \code{VaR} for each counterparty.}
    \item{\code{ES.cont}:}{is a numeric vector with the risk contributions to the last entry in \code{ES} for each counterparty.}
    \item{\code{ES.tau.cont}:}{is a numeric vector with the risk contributions to TAU, corresponding to the the last entry in \code{ES} for each counterparty.}
    \item{\code{SD.cont}:}{is a numeric vector with the risk contributions to the portfolio standard deviation.}


    \bold{loss distribution:}\cr
    \item{\code{loss}:}{is a numeric vector with the different losses / exposure bands, the PDF is calculated for.}
    \item{\code{PDF}:}{is a numeric vector with the probability density function.}
    \item{\code{CDF}:}{is a numeric vector with the cumulative distribution function.}

 
     \bold{providing model input:}\cr
    \item{\code{input}:}{is a temporary list, used if input data (portfolio, rating.scale, sec.var) are passed directly to \code{init}.}
    \item{\code{path.in}:}{is a character string with the path to the directory, where are the input files. All input files have to be in this directory. The path should end with "...\\\\" or ".../".}
    \item{\code{port.name}:}{is a character string with the name of the portfolio file, ending with ".csv".}
    \item{\code{rating.scale.name}:}{is a character string with the name of the rating file, ending with ".csv".}
    \item{\code{sec.var.name}:}{is a character string with the name of the file containing the sector variances, ending with ".csv".}
    \item{\code{file.format}:}{is a character string defining the format of the input files if no data frames are provided. You can choose between 'csv', which means that the separation character is ',' and the decimal character is '.' and 'csv2', which means that the separation character is ';' and the decimal character is ','.}
    
    
    \bold{model output:}\cr
    \item{\code{export.to.file}:}{logical, defining if results should be exported to \code{path.out}}
    \item{\code{path.out}:}{is a character string with the path to the directory, where the output should be written to if \code{export.to.file} = \code{TRUE}, ending with "...\\\\" or ".../".
    Be aware, that actually, by writing any output, the model will create a subdirectory in \code{path.out} with its name. So you can use \code{path.in} also as \code{path.out} (which is the default case), create different models from the same input data (or different files, lying in the same directory), without worrying about the output path or overwriting other results.}
 
 
 
    \bold{internal model parameters:}\cr
    \item{\code{a}:}{is a numeric value calculated during \code{loss.dist}, necessary for the PDF and needed in \code{rc.vares}.}
    \item{\code{B}:}{is a matrix calculated during \code{loss.dist}, necessary for the PDF and needed in \code{rc.vares}}
    \item{\code{M}:}{is a numeric value giving the maximal exposure band, which is calculated during \code{loss.dist()}.}
    \item{\code{read.OK}:}{is a flag indicating if reading input files / data frames was successful.}
    \item{\code{plausi.OK}:}{is a flag indicating if the plausibility check was successful.}
    \item{\code{rc.OK}:}{is a flag indicating if the calculation of risk contributions to VaR and ES was successful.}
    \item{\code{VaR.pos}:}{is a numeric vector with the positions of \code{VaR} in \code{CDF}.}
    \item{\code{alpha.crp}:}{is a numeric vector containing the CDF-levels of \code{VaR}. Because they are from the calculated CDF \code{alpha.crp} will always be a little bit greater as \code{alpha}. With the help of \code{alpha.crp} it is not necessary to store the CDF for calculating risk contributions to VaR and ES (\code{save.memory} = \code{TRUE} can be used.}
    \item{\code{name}:}{is a character string with the name of the model. This slot is set at the time you run the first method on your model, not by creation via \code{init}. If you change the model name (the name of the R object) \code{name} is updated automatically by the first method running on it.}
    \item{\code{changes.read}:}{is an internal flag indicating changes on input parameters (by call to \code{<slot>}) affecting \code{read()}.}
    \item{\code{changes.plausi}:}{is an internal flag indicating changes on input parameters (by call to \code{<slot>}) affecting \code{plausi()}.}
    \item{\code{changes.calc.portfolio.statistics}:}{is an internal flag indicating changes on input parameters (by call to \code{<slot>}) affecting \code{calc.portfolio.statistics}.}
    \item{\code{changes.loss}:}{is an internal flag indicating changes on input parameters (by call to \code{<slot>}) affecting \code{loss.dist}.}
    \item{\code{changes.measure}:}{is an internal flag indicating changes on input parameters (by call to \code{<slot>}) affecting \code{measure}.}
    \item{\code{changes.plot}:}{is an internal flag indicating changes on input parameters (by call to \code{<slot>}) affecting \code{plot}.}
    \item{\code{changes.rc.vares}:}{is an internal flag indicating changes on input parameters (by call to \code{<slot>}) affecting \code{rc.sd}.}
    \item{\code{changes.rc.sd}:}{is an internal flag indicating changes on input parameters (by call to \code{<slot>}) affecting \code{rc.vares}.}
    \item{\code{changes.export}:}{is an internal flag indicating changes on input parameters (by call to \code{<slot>}) affecting \code{export}.}
  }
}
\section{Methods}{
  \describe{
    \item{calc.portfolio.statistics}{Calculating portfolio statistics}
    \item{loss.dist}{Calculating the loss distribution}
    \item{rc.sd}{Calculating risk contributions to standard deviation}
    \item{rc.vares}{Calculating risk contributions to VaR and ES}
    \item{crp.CSFP}{Main routine for CSFP-model}
    \item{export}{Export risk contributions}
    \item{measure}{Calculating portfolio measures}
    \item{plausi}{Checking input data for plausibility}
    \item{plot}{Plotting the PDF}
    \item{read}{Reading the input files}
    \item{summary}{Summarize portfolio key numbers}
    \item{write.summary}{Writing summary to file}
    \item{<slot>}{You can enter every slot via \code{<slot>} e.g. \code{VaR(MyModel)} gives the calculated value at risk.}
    \item{<slot> <-}{You can change the value of a slot if this is an input parameter via \code{<slot> <-} e.g. \code{alpha(MyModel)<-c(0.999,0.9995)}. You can do so with all slots that are available in \code{init} and the slots containing portfolio information (\code{CP.NR}, \code{NEX}, \code{CP.rating}, \code{LGD}, \code{W}), rating information (\code{rating}, \code{rating.PD}, \code{rating.PD}) and the sector variances \code{sec.var} }
   }
}
\references{
First Boston Financial Products, "CreditRisk+", 1997 \cr
Gundlach & Lehrbass, "CreditRisk+ in the Banking Industry", Springer, 2003
}
\seealso{
\code{\link{init}}
\code{\link{crp.CSFP}}
}
\author{
Kevin Jakob & Dr. Matthias Fischer
}
\examples{
MyModel=init(path.in=system.file("data",package="crp.CSFP"),loss.unit=1e6,
calc.rc=TRUE)

# or pass portfolio directly and use random sector variances
Path=system.file("data",package="crp.CSFP")
portfolio=read.csv(paste(Path,"/portfolio.csv",sep=""))
rating.scale=read.csv(paste(Path,"/rating_pd.csv",sep=""))
sec.var=data.frame(Var=runif(3,0,2))

MyModel=init(portfolio=portfolio,rating.scale=rating.scale,sec.var=sec.var,
loss.unit=1e6,calc.rc=TRUE)

# execute portfolio calculation
MyModel=crp.CSFP(MyModel)
}
\keyword{classes}
