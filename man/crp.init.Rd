\name{crp.init}
\alias{crp.init}
\title{Initializing a new entity of class crp.CSFP}
\description{
This function helps to create a new entity of a crp.CSFP object. The arguments, given to the functions become the attributes of the new model.
}
\usage{
crp.init(PATH.IN, PATH.OUT, PORT.NAME = "portfolio.csv",
RISK.NAME = "rating_pd.csv", PDVAR.NAME = "pd_sector_var.csv", SEC.VAR.EST = 5,
LOSS.UNIT = 1e+06, NITER.MAX = 0.9999, NITER.MAX.GLOBAL = 1e+05,
ALPHA = c(0.999), PLOT.PDF = TRUE, CALC.RISK.CONT = FALSE, PLOT.SCALE = 1e+06,
PLOT.RANGE.X = c(0, 0), PLOT.RANGE.Y = c(0, 0), save.memory = FALSE,
file.format = "csv", portfolio=data.frame(), risk.matrix=data.frame(),
sec.var=data.frame())
}
\arguments{
  \item{PATH.IN}{
is a character string with the path to the directory, where are the input files. All input files have to be in this directory. It must end with ".\\\\".
In alternative the files can be passed as dataframes to \code{crp.init}, please have a look at \code{portfolio}, \code{risk.matrix} or \code{sec.var}.
}
  \item{PATH.OUT}{
is a character string with the path to the directory, where the output should be written to, ending with ".\\\\".  The default for \code{PATH.OUT} is \code{PATH.IN}.
Be aware, that actually, by writing any output, the model will create a subdirectory in \code{PATH.OUT} with its name. So you can use your \code{PATH.IN} as \code{PATH.OUT} (which is the default case), create different models from the same input data (or from different files, lying in the same directory), without worrying about the output path or overwriting other results.
}
\item{portfolio}{
 is a dataframe containing the portfolio information. The structure has to be the same as described in \code{PORT.NAME} for the .csv files. If not provided, the file is read from \code{PATH.IN}.
 }
 \item{risk.matrix}{
 is a dataframe containing the rating masterscale. The structure has to be the same as described in \code{RISK.NAME} for the .csv files. If not provided, the file is read from \code{PATH.IN}.
 }
 \item{sec.var}{
 is a dataframe containing the sector variances. The structure has to be the same as described in \code{PDVAR.NAME} for the .csv files. If not provided, the file is read from \code{PATH.IN}.
 }
  \item{PORT.NAME}{
is a character string with the name of the portfolio file, ending with ".csv". The file must contain the following columns: CPnumber, CPname, exposure,lgd, maturity, rating, S1, S2 ,... . Take care of the right spelling of the column titles and capitalization.
}
  \item{RISK.NAME}{
is a character string with the name of the rating file, ending with ".csv". The file must contain the following columns: RATING, PD, SD. The SD column is not necessary if \code{SEC.VAR.EST}=5. The RATING classes have to be integer. A class '0' is allowed to use. All Counterparties in this class are removed before analysing the portfolio. Take care of the right spelling of the column titles and capitalization.
}
  \item{PDVAR.NAME}{
is a character string with the name of the file containing the sector variances, ending with ".csv". The file must contain the column Var. Take care of the right spelling and capitalization. The file is not used if \code{SEC.VAR.EST} != 5.
}
  \item{SEC.VAR.EST}{
is an indicator for the mode, the sector standard deviations should be calculated.\cr
1: \code{Sum of (weights * SD)}\cr
2: \code{[Sum of (weights * SD)] / MU(k)}\cr
3: \code{Sum of (sqrt(weights) * SD)}\cr
4: \code{[Sum of ((sqrt(weights) * SD)] / MU(k)}\cr
5: \code{Read variances from external file}\cr
with: \code{MU.(k)=sum(W[,k+1]*PD)}
}
  \item{LOSS.UNIT}{
is the discretisation parameter for net exposures.
}
  \item{NITER.MAX}{
is the maximum number of exposure bands/probabilities being calculated. If a number smaller one is insert, the calculation stops at this level of the CDF. For this mode, an upper bound of \code{NITER.MAX.GLOBAL} is implemented to stop if this number of iterations is reached. If the desired confidence level is not reached till this threshold, a warning comes up. If you set \code{NITER.MAX} > \code{NITER.MAX.GLOBAL} manually, the threshold will be ignored.
}
  \item{NITER.MAX.GLOBAL}{
is the maximum number of iterations for \code{NITER.MAX} < 1. Be aware, that a high value can have high memory costs during the algorithm, even if the calculation of the CDF stops very much earlier.
}
  \item{ALPHA}{
is the vector of confidence levels (between 0 and 1), the Value at risk and expected shortfall should be calculated. It should be no problem if the entries are not in an ascending order. For the risk contributions only the last entry will be considered. 
}
  \item{PLOT.PDF}{
is a logical indicator for plotting the PDF or not. It will not be recognized if you start \code{crp.plot} directly.
}
  \item{CALC.RISK.CONT}{
is a flag for calculating the risk contributions or not. It will not be recognized if you start \code{crp.CSFP.rc.vares} or \code{crp.CSFP.rc.sd} directly.
}
  \item{PLOT.SCALE}{
is a numeric value defining the scale for the horizontal axis (the losses) of the plot of the PDF.
}
  \item{PLOT.RANGE.X}{is a numeric vector with two entries representing the range on the x-axis (the losses) for the plot of the loss distribution. If you insert values smaller one, this will be interpreted as levels of the CDF. The defaults for PLOT.RANGE.X/-Y (0,0) have the meaning, that R will choose axis range by itself.
}
  \item{PLOT.RANGE.Y}{
is the same as \code{PLOT.RANGE.X} above for vertical axis.
}
  \item{save.memory}{
is a switch for the save memory mode. If \code{save.memory}=TRUE, \code{LOSS} and \code{CDF} will not be stored permanently. Instead they are erased after exporting the loss distribution. If needed, the CDF is recalculated from the PDF. The same happens to \code{a} and \code{B} at the end of \code{crp.CSFP.loss} if \code{CALC.RISK.CONT}=FALSE. If \code{save.memory} is FALSE all data will be kept.
}
  \item{file.format}{
is a character string defining the format of the input files. You can choose between 'csv', which means that the separation character is ',' and the decimal character is '.' and 'csv2', which means that the separation character is ';' and the decimal character is ','.
}
}

\value{
A new object of class crp.CSFP.
}
\author{
Dr. Matthias Fischer, Kevin Jakob & Stefan Kolb
}

\seealso{
\code{\linkS4class{crp.CSFP}}
}
\examples{
MyModel=crp.init(PATH.IN=system.file("data",package="crp.CSFP"),LOSS.UNIT=1e6,
CALC.RISK.CONT=TRUE)

# or pass portfolio directly and use random sector variances
Path=system.file("data",package="crp.CSFP")
portfolio=read.csv(paste(Path,"/portfolio.csv",sep=""))
risk.matrix=read.csv(paste(Path,"/rating_pd.csv",sep=""))
sec.var=data.frame(Var=runif(3,0,2))

MyModel=crp.init(portfolio=portfolio,risk.matrix=risk.matrix,sec.var=sec.var,
LOSS.UNIT=1e6,CALC.RISK.CONT=TRUE,PATH.OUT=Path)
}