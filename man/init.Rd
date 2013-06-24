\name{init}
\alias{init}
\alias{crp.init}
\title{Initializing a new entity of class crp.CSFP}
\description{
This function helps to create a new entity of a crp.CSFP object. The arguments, given to the functions become the attributes of the new model.
}
\usage{
init(path.in = "", path.out = "", port.name = "portfolio.csv",
rating.scale.name = "rating_pd.csv", sec.var.name = "pd_sector_var.csv",
sec.var.est = 5, loss.unit = 1e+06, Niter.max = 0, alpha.max = 0.9999,
Niter.max.global = 1e+05, alpha = c(0.999), PLOT.PDF = TRUE,
export.to.file = FALSE, calc.rc = FALSE, PLOT.scale = 1e+06,
PLOT.range.x = c(0, 0), PLOT.range.y = c(0, 0), save.memory = FALSE,
file.format = "csv", portfolio = data.frame(), rating.scale = data.frame(),
sec.var = data.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{portfolio}{
is a data frame containing the portfolio information. The structure has to be the same as described in \code{port.name} for the .csv files. If not provided, the file is read from \code{path.in}.
}
  \item{rating.scale}{
is a data frame containing the rating master scale. The structure has to be the same as described in \code{rating.scale.name} for the .csv files. If not provided, the file is read from \code{path.in}.
}
  \item{sec.var}{
is a data frame containing the sector variances. The structure has to be the same as described in \code{sec.var.name} for the .csv files. If not provided, the file is read from \code{path.in}.
}
\item{path.in}{
is a character string with the path to the directory, where are the input files. All input files have to be in this directory. It must end with "...\\\\" or ".../".
In alternative the files can be passed as data frames to \code{init}, please have a look at \code{portfolio}, \code{rating.scale} or \code{sec.var} below.
}
\item{file.format}{
is a character string defining the format of the input files. You can choose between 'csv', which means that the separation character is ',' and the decimal character is '.' and 'csv2', which means that the separation character is ';' and the decimal character is ','.}
  \item{path.out}{
is a character string with the path to the directory, where the output should be written to if \code{export.to.file} = \code{TRUE}, ending with "...\\\\" or ".../".
Be aware, that actually, by writing any output, the model will create a subdirectory in \code{path.out} with its name. So you can use your \code{path.in} as \code{path.out} (which is the default case), create different models from the same input data (or from different files, lying in the same directory), without worrying about the output path or overwriting other results.
}
  \item{port.name}{
is a character string with the name of the portfolio file, ending with ".csv". The file must contain the following columns: CPnumber, CPname, exposure,lgd, maturity, rating, S1, S2 ,... . Take care of the right spelling of the column titles and capitalization.}
  \item{rating.scale.name}{
is a character string with the name of the rating file, ending with ".csv". The file must contain the following columns: RATING, PD, SD. The SD column is not necessary if \code{sec.var.est}=5. The rating classes have to be integer values. A class '0' is allowed. All counterparties in this class are removed before analyzing the portfolio. Take care of the right spelling of the column titles and capitalization.}
  \item{sec.var.name}{
is a character string with the name of the file containing the sector variances, ending with ".csv". The file must contain the column Var. Take care of the right spelling and capitalization. The file is not used if \code{sec.var.est} != 5.
}
  \item{sec.var.est}{
is an indicator for the mode, the sector standard deviations should be calculated.\cr
1: \code{Sum of (weights * SD)}\cr
2: \code{[Sum of (weights * SD)] / MU(k)}\cr
3: \code{Sum of (sqrt(weights) * SD)}\cr
4: \code{[Sum of ((sqrt(weights) * SD)] / MU(k)}\cr
5: \code{Read variances from external file}\cr
with: \code{MU.(k)=sum(W[,k+1]*PD)}}
  \item{loss.unit}{
is the discretization parameter for net exposures.}
  \item{Niter.max}{
is the maximum number of exposure bands/probabilities being calculated.}
  \item{alpha.max}{in alternative to \code{Niter.max}, one can also define the maximum CDF level.}
  \item{Niter.max.global}{
is the maximum number of iterations if \code{alpha.max} is provided. Be aware, that a high value can have high memory costs during the algorithm, even if the calculation of the CDF stops very much earlier.}
  \item{alpha}{
is the vector of confidence levels (between 0 and 1), the Value at risk and expected shortfall should be calculated. It should be no problem if the entries are not in an ascending order. For the risk contributions only the last entry will be considered. }
  \item{PLOT.PDF}{
is a logical indicator for plotting the PDF or not. It will not be recognized if you start \code{plot} directly.}
  \item{export.to.file}{
is a logical indicator defining if loss distribution, risk contributions and a summary  should be exported to \code{path.out}.}
  \item{calc.rc}{
is a flag for calculating the risk contributions or not. It will not be recognized if you start \code{rc.vares} or \code{rc.sd} directly.
}
  \item{PLOT.scale}{
is a numeric value defining the scale for the horizontal axis (the losses) of the plot of the PDF.}
  \item{PLOT.range.x}{
is a numeric vector with two entries representing the range on the x-axis (the losses) for the plot of the loss distribution. If you insert values smaller one, this will be interpreted as levels of the CDF. The defaults for \code{PLOT.range.x /-.y} (0,0) have the meaning, that R will choose axis range by itself.}
  \item{PLOT.range.y}{
is the same as \code{PLOT.range.x} above for vertical axis.}
  \item{save.memory}{
is a switch for the save memory mode. If \code{save.memory}=TRUE, \code{loss} and \code{CDF} will not be stored permanently. If needed, the CDF is recalculated from the PDF. The same happens to \code{a} and \code{B} at the end of \code{loss.dist} if \code{calc.rc}=FALSE.}
}
\value{
A new object of class crp.CSFP.
}
\author{
Kevin Jakob & Dr. Matthias Fischer
}

\seealso{
\code{\linkS4class{crp.CSFP}}
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
}
