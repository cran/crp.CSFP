crp.round <- function(a){ifelse(a%%0.5==0&a%%1!=0&trunc(a)%%2==0,round(a,0)+1,round(a,0))}

crp.init <- function(PATH.IN,PATH.OUT,PORT.NAME="portfolio.csv",RISK.NAME="rating_pd.csv",PDVAR.NAME="pd_sector_var.csv",SEC.VAR.EST=5,LOSS.UNIT=1e6,NITER.MAX=0.9999,NITER.MAX.GLOBAL=1e5,ALPHA=c(0.9995),PLOT.PDF=TRUE,CALC.RISK.CONT=FALSE,PLOT.SCALE=1e6,PLOT.RANGE.X=c(0,0),PLOT.RANGE.Y=c(0,0),save.memory=FALSE,file.format="csv"){
 
  cat("    CreditRisk+ portfolio model \n    Copyright (C) 2011  Dr. Matthias Fischer, Kevin Jakob & Stefan Kolb

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    version 2 as published by the Free Software Foundation.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA  02110-1301, USA.\n")
  
  if(missing(PATH.IN))                                                                               # if no PATH.IN is insert, open window to 
    PATH.IN=paste(choose.dir(),"\\",sep="")                                                          # choose directory manuelly
  if(missing(PATH.OUT))
    PATH.OUT=PATH.IN
  if(!missing(file.format)){
    if(!(file.format=="csv" || file.format=="csv2")){
      cat("Wrong specification of file.format. Please choose between csv (sep= , dec= . ) and csv2 (sep= ; dec= , ).\n")
      file.format="csv"
    }
  }
  
  return(new("crp.CSFP",PATH.IN=PATH.IN,PATH.OUT=PATH.OUT,PORT.NAME=PORT.NAME,RISK.NAME=RISK.NAME, PDVAR.NAME=PDVAR.NAME,SEC.VAR.EST=SEC.VAR.EST,LOSS.UNIT=LOSS.UNIT,NITER.MAX=NITER.MAX,NITER.MAX.GLOBAL=NITER.MAX.GLOBAL,ALPHA=ALPHA,PLOT.PDF=PLOT.PDF,CALC.RISK.CONT=CALC.RISK.CONT,PLOT.SCALE=PLOT.SCALE, PLOT.RANGE.X=PLOT.RANGE.X,PLOT.RANGE.Y=PLOT.RANGE.Y,save.memory=save.memory,file.format=file.format))

}

fo <- function(x){                                                                                   # function formatting the output of big numbers
  s=""
  s1=""
  for(i in 1:length(x)){
    if(abs(x[i])>=1e12)
      s1=paste(round(x[i]/1e12,2),"Tril.")
    else if(abs(x[i])>=1e9)
      s1=paste(round(x[i]/1e9,2),"Bil.")
    else if(abs(x[i])>=1e6)
      s1=paste(round(x[i]/1e6,2),"Mio.")
    else if(abs(x[i])>=1e3)
      s1=paste(round(x[i]/1e3,2),"Thd.")
    else
      s1=paste(round(x[i],2))
    s=paste(s,s1)
  }
  return(s)
}
