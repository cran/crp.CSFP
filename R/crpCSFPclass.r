setClass(Class="crp.CSFP",                                                                               #define the names and types of attributes of crp.CSFP
	   representation=representation(
							PATH.IN = "character",
							PATH.OUT="character",
							PORT.NAME="character",
							RISK.NAME="character",
							PDVAR.NAME="character",
							SEC.VAR.EST="numeric",
							LOSS.UNIT="numeric",
							NITER.MAX="numeric",
              NITER.MAX.GLOBAL="numeric",
							ALPHA="numeric",
							PLOT.PDF="logical",
							CALC.RISK.CONT="logical",
							rating="numeric",
							rating.PD="numeric",
							rating.SD="numeric",
							NS="numeric",
							NC="numeric",
							SEC.VAR="numeric",
							CP.NR="numeric",
							CP.RATING="numeric",
							NEX="numeric",
							LGD="numeric",
							PL0="numeric",
							PD0="numeric",
							EL="numeric",
							EL.crp="numeric",
							NU="numeric",
							PL="numeric",
							PD="numeric",
							M="numeric",
							MU.K="numeric",
							V.K="numeric",
              SIGMA.K="numeric",
							SIGMA2_DIV="numeric",
							SIGMA2_SYST="numeric",
							SD="numeric",
							SD.crp="numeric",
							W="matrix",
							ALPHA.MAX="numeric",
							a="numeric",
							PDF="numeric",
							CDF="numeric",
							B="matrix",
							LOSS="numeric",
              PLOT.SCALE="numeric",
							PLOT.RANGE.X="numeric",
							PLOT.RANGE.Y="numeric",
							VaR="numeric",
              EC="numeric",
							ES="numeric",
							VaR.CONT="numeric",
							ES.CONT="numeric",
							ES.TAU.CONT="numeric",
							SD.CONT="numeric",
							read.OK="logical",
							plausi.OK="logical",
              rc.OK="logical",
              save.memory="logical",
              SELV="numeric",
              ALPHA.crp="numeric",
              NAME="character",
              changes.crp.read="logical",
              changes.crp.plausi="logical",
              changes.crp.calc.portfolio.statistics="logical",
              changes.crp.CSFP.loss="logical",
              changes.crp.measure="logical",
              changes.crp.plot="logical",
              changes.crp.CSFP.rc.vares="logical",
              changes.crp.CSFP.rc.sd="logical",
              changes.crp.export="logical",
              file.format="character"),
	   prototype=prototype( PATH.IN="C:\\",                                                            #define default values
					PATH.OUT="C:\\",
					PORT.NAME="portfolio.csv",
					RISK.NAME="rating_pd.csv",
					PDVAR.NAME="pd_sector_var.csv",
					SEC.VAR.EST=5,
					LOSS.UNIT=1e6,
					NITER.MAX=0.9999,
          NITER.MAX.GLOBAL=1e5,                
					ALPHA=c(0.9995),
					PLOT.PDF=TRUE,
					CALC.RISK.CONT=FALSE,
					rating=0,
					rating.PD=0,
					rating.SD=0,
					NS=0,
					NC=0,
					SEC.VAR=0,
					CP.NR=0,
					CP.RATING=0,
					NEX=0,
					LGD=0,
					PL0=0,
					PD0=0,
					EL=0,
					EL.crp=0,
					NU=0,
					PL=0,
					PD=0,
					M=0,
					MU.K=0,
					V.K=0,
					SIGMA.K=0,
					SIGMA2_DIV=0,
					SIGMA2_SYST=0,
					SD=0,
					SD.crp=0,
					W=matrix(),
					ALPHA.MAX=0,
					a=0,
					PDF=0,
					CDF=0,
					B=matrix(),
					LOSS=0,
          PLOT.SCALE=1e6,                
					PLOT.RANGE.X=c(0,0),
					PLOT.RANGE.Y=c(0,0),
					VaR=0,
          EC=0,
					ES=0,
					VaR.CONT=0,
					ES.CONT=0,
					ES.TAU.CONT=0,
					SD.CONT=0,
          read.OK=FALSE,
          plausi.OK=FALSE,
          rc.OK=FALSE,
          save.memory=FALSE,
          SELV=0,
          ALPHA.crp=0,
          NAME="MyModel",
          changes.crp.read=FALSE,
          changes.crp.plausi=FALSE,
          changes.crp.calc.portfolio.statistics=FALSE,
          changes.crp.CSFP.loss=FALSE,
          changes.crp.measure=FALSE,
          changes.crp.plot=FALSE,
          changes.crp.CSFP.rc.vares=FALSE,
          changes.crp.CSFP.rc.sd=FALSE,
          changes.crp.export=FALSE,
          file.format="csv")
)

setGeneric("crp.read",function(this) standardGeneric("crp.read"))
setMethod("crp.read",c("crp.CSFP"),function(this) {
#
#       <crp.read>      Import portfolio and risk data
#
#       Last Modified:  07/12/2011
#       Author:         Matthias Fischer & Kevin Jakob, BLB        
#

   ERROR=0
# --------------------------------------------------------------------------------------
#               Importing rating, probability of default and standard deviation
# ---------------------------------------------------------------------------------------
  
  cat("Importing risk information (Rating, PD, SD)....\n")
  if(file.exists(paste(this@PATH.IN,this@RISK.NAME,sep=""))==TRUE){                                  # check if the file exists
    if(this@file.format=="csv")
      RISK.MATRIX=read.csv(paste(this@PATH.IN,this@RISK.NAME,sep=""),header=TRUE)
    else if(this@file.format=="csv2")
      RISK.MATRIX=read.csv2(paste(this@PATH.IN,this@RISK.NAME,sep=""),header=TRUE)
    if(is.null(RISK.MATRIX$RATING)){
      cat("The RATING-column in ",this@RISK.NAME," is missing.")
      ERROR=ERROR+1
    }
    else
      this@rating=RISK.MATRIX$RATING                                                                 # assigne data to attributes
    if(is.null(RISK.MATRIX$PD)){
      cat("The PD-column in ",this@RISK.NAME," is missing.\n")
      ERROR=ERROR+1
    }
    else
      this@rating.PD=RISK.MATRIX$PD
    if(this@SEC.VAR.EST<5){
      if(is.null(RISK.MATRIX$SD)){
        cat("The SD-column in ",this@RISK.NAME," is missing.\n")
        ERROR=ERROR+1
      }
      else
        this@rating.SD=RISK.MATRIX$SD
    }
    remove(RISK.MATRIX)
  }
  else{
	  cat(paste("ERROR: The ",this@RISK.NAME," file is missing!!!\n",sep=""))
    ERROR=ERROR+1
  }
  i=which(this@rating==0)
  if(length(i)>0){
    this@rating=this@rating[-i]
    this@rating.PD=this@rating.PD[-i]
    this@rating.SD=this@rating.SD[-i]
  }
   


# --------------------------------------------------------------------------------------
#               Importing portfolio data
# ---------------------------------------------------------------------------------------

  cat("Importing portfolio data....\n")
  if(file.exists(paste(this@PATH.IN,this@PORT.NAME,sep=""))==TRUE){                                  # check if file exists
    if(this@file.format=="csv")
   	  PORTFOLIO=read.csv(paste(this@PATH.IN,this@PORT.NAME,sep=""),header=TRUE)
    else if(this@file.format=="csv2")
      PORTFOLIO=read.csv2(paste(this@PATH.IN,this@PORT.NAME,sep=""),header=TRUE)
    this@NS=length(PORTFOLIO)-6                                                                      # assigne data to attributes

    if(is.null(PORTFOLIO$CPnumber)){
      cat("The CPnumber-column in ",this@PORT.NAME," is missing.\n")
      ERROR=ERROR+1
    }
    else{
      this@CP.NR=PORTFOLIO$CPnumber
      this@NC=length(PORTFOLIO$CPnumber)
    }
    if(is.null(PORTFOLIO$rating)){
      cat("The rating-column in ",this@PORT.NAME," is missing.\n")
      ERROR=ERROR+1
    }
    else
   	  this@CP.RATING=PORTFOLIO$rating
   	if(is.null(PORTFOLIO$exposure)){
      cat("The exposure-column in ",this@PORT.NAME," is missing.\n")
      ERROR=ERROR+1
    }
    else
   	  this@NEX=PORTFOLIO$exposure
    if(is.null(PORTFOLIO$lgd)){
      cat("The lgd-column in ",this@PORT.NAME," is missing.\n")
      ERROR=ERROR+1
    }
    else
   	  this@LGD=PORTFOLIO$lgd
    TEMP=(do.call(cbind,PORTFOLIO))[,-(1:6)]                                                         # extract weight matrix
    if(is.null(TEMP)){
      cat("The sector-weights in ",this@PORT.NAME," are missing.\n")
      ERROR=ERROR+1
    }
    else{
      remove(PORTFOLIO)                                                                                
      if(this@NS==1)
        TEMP=matrix(ncol=1,TEMP)                                                                       # convert to matrix
      this@W=cbind(1-apply(TEMP,1,sum),TEMP)                                                           # attache idiosyncratic weights as first column
      remove(TEMP)
    }
   }
   else{
	 cat(paste("ERROR: The ",this@PORT.NAME," file is missing!!!\n"),sep="")
      ERROR=ERROR+1
   }
   
   
   emptyCP=0
   l=0
   for(i in 1:this@NC){
     if(this@CP.RATING[i]==0){
       l=l+1
       emptyCP[l]=i
     }
     else if(this@NEX[i]==0 || this@rating.PD[this@CP.RATING[i]]==0 || this@LGD[i]==0){
       l=l+1
       emptyCP[l]=i
     }
   }
   if(l>0){
     this@CP.NR=this@CP.NR[-emptyCP]
     this@CP.RATING=this@CP.RATING[-emptyCP]
     this@NEX=this@NEX[-emptyCP]
     this@LGD=this@LGD[-emptyCP]
     this@W=this@W[-emptyCP,]
     this@NC=this@NC-l
   }
   
   cat(this@NS,"sectors ...")
   cat(this@NC,"counterparties (",l," removed) ... OK\n",sep="")
   
   
# --------------------------------------------------------------------------------------
#               Importing sector information
# ---------------------------------------------------------------------------------------

   if(this@SEC.VAR.EST==5){                                                                          # sector variances are just needed if SEC.VAR.EST=5, otherwise they are calculated out of rating.SD
   	cat("Importing PD sector variances....\n")   
      if(file.exists(paste(this@PATH.IN,this@PDVAR.NAME,sep=""))==TRUE){                             # check if file exists
        if(this@file.format=="csv")
   		    SEC.VAR=read.csv(paste(this@PATH.IN,this@PDVAR.NAME,sep=""),header=TRUE)
        else if(this@file.format=="csv2")
          SEC.VAR=read.csv2(paste(this@PATH.IN,this@PDVAR.NAME,sep=""),header=TRUE)
        if(is.null(SEC.VAR$Var)){
          cat("The Var-column in ",this@PDVAR.NAME," is missing.\n")
        ERROR=ERROR+1
        }
        else if(length(SEC.VAR$Var)<this@NS){
          cat("ERROR: Number of sector variances specified in ",this@PDVAR.NAME," (",length(SEC.VAR$Var),") is smaller then the number of sectors (",this@NS,").\n",sep="")
          ERROR=ERROR+1
        }
        else{
   		    this@SEC.VAR=SEC.VAR$Var
   		    remove(SEC.VAR)
        }
   		}
   	  else{
		    cat(paste("ERROR: The ",this@PDVAR.NAME," file is missing!!!\n",sep=""))
        ERROR=ERROR+1
   	  }
               
   }
   if(this@SEC.VAR.EST<5)
	   this@SEC.VAR=0

   if(ERROR>0)                                                                                       # set state read.ok
     this@read.OK=FALSE
   else
     this@read.OK=TRUE

   this@changes.crp.read=FALSE
   gc()
   return(this)
}
)

setGeneric("crp.plausi",function(this) standardGeneric("crp.plausi"))
setMethod(f="crp.plausi",signature=c("crp.CSFP"),definition=function(this){
#
#       <crp.plausi>    Verify that the input data makes sence
#
#       Last Modified:  07/12/2011
#       Author:         Dr. Matthias Fischer, Stefan Kolb & Kevin Jakob
#

  if(this@changes.crp.read)
    cat("WARNING: Inputparameter effecting crp.read have been changed, without running crp.read and the following routines afterwards.\n")

  ERROR=0
  if(length(this@rating.PD)>1){                                                                     # check that higher rating has higher probability of default
    for(i in 1:(length(this@rating.PD)-1)){
      if(this@rating.PD[i+1]-this@rating.PD[i]<0){
        cat("ERROR: Higher Rating cannot have smaller Probability of Default!!!\n")
        ERROR=ERROR+1
      }
    }
  }

  if(sum(this@NEX>=0)!=length(this@NEX)){                                                            # check if exposure is not negative
    cat("ERROR: Exposure has to be not negative. \n")
    ERROR=ERROR+1
  }
  
  if(this@NS>1){                                                                                     # check if sector weights are plausible
    if(sum(rowSums(this@W[,-1])<=1)!=nrow(this@W[,-1])|sum(this@W[,-1]>=0)!=nrow(this@W[,-1])*ncol(this@W[,-1])){
      cat("ERROR: Weights for each exposure have to be between 0 and 1!!!\n")
      ERROR=ERROR+1
     }
  }
  else if(this@NS==1){
	  if(any(this@W>=0 & this@W<=1)==FALSE){
		  cat("ERROR: Weights for each exposure have to be between 0 and 1!!!\n")
      ERROR=ERROR+1
	  }
  }

  if(this@SEC.VAR.EST<5){
    if(ERROR>0)
      this@plausi.OK=FALSE
    else
      this@plausi.OK=TRUE
    gc()
	  return(this)                                                                                     # return if the variance is calculated and not from an input file
  }                                                       

  if(sum(this@SEC.VAR>=0)!=length(this@SEC.VAR)){                                                    # check if sector variances are postive
    cat("ERROR: Sector Variance has to be positive\n")
    ERROR=ERROR+1
  }
  if(ERROR>0)
    this@plausi.OK=FALSE
  else
    this@plausi.OK=TRUE
  gc()
  return(this)                                                                        
}
)

setGeneric("crp.calc.portfolio.statistics",function(this) standardGeneric("crp.calc.portfolio.statistics"))
setMethod(f="crp.calc.portfolio.statistics",signature=c("crp.CSFP"),definition=function(this){
#
#       <crp.calc.portfolio.statistics>              calculate key numbers for portfolio
#
#       Last Modified:  03/11/2011
#       Author:         Matthias Fischer & Kevin Jakob        
#

  if(this@changes.crp.read)
    cat("WARNING: Inputparameter effecting crp.read have been changed, without running crp.read and the following routines afterwards.\n")
  if(this@changes.crp.plausi)
    cat("WARNING: Inputparameter effecting crp.plausi have been changed, without running crp.plausi and the following routines afterwards.\n")
  

	this@PL0=this@NEX*this@LGD                                                                         # potential loss
  this@PD0=this@rating.PD[this@CP.RATING]                                                            # probability of default
  this@NU=crp.round(this@PL0/this@LOSS.UNIT)                                                         # integer muliples of LOSS.UNIT representing poZential loss
	this@NU=ifelse(this@NU==0,1,this@NU)                                                               # should not be 0
	this@PL=this@NU*this@LOSS.UNIT                                                                     # discretized potential loss
  this@PD=this@PD0*(this@PL0/this@PL)                                                                # transformed PD
  
 	cat("Calculating sector information...\n")

  for(k in 1:this@NS){
    TEMP=this@W[,1+k]
    SELECTION=which(TEMP>0)
    this@MU.K[k]=sum(TEMP[SELECTION]*this@PD[SELECTION])                                             # formula (39), CSFP 1997, p. 43
    this@V.K[k]=sum(TEMP[SELECTION]*this@PD[SELECTION]*this@PL[SELECTION])                           # required for risk contributions
    if(this@SEC.VAR.EST==1)
      this@SIGMA.K[k]=sum(TEMP[SELECTION]*this@rating.SD[this@CP.RATING[SELECTION]])                 # (2.21) p. 17 in Gundlach/Lehrbass (2003)
    else if(this@SEC.VAR.EST==2)
      this@SIGMA.K[k]=sum(TEMP[SELECTION]*this@rating.SD[this@CP.RATING[SELECTION]])/this@MU.K[k]    # (2.21) p. 17 in Gundlach/Lehrbass (2003)
    else if(this@SEC.VAR.EST==3)
      this@SIGMA.K[k]=sum(sqrt(TEMP[SELECTION])*this@rating.SD[this@CP.RATING[SELECTION]])           # (++++) p. 18 in Gundlach/Lehrbass (2003)
    else if(this@SEC.VAR.EST==4)
      this@SIGMA.K[k]=sum(sqrt(TEMP[SELECTION])*this@rating.SD[this@CP.RATING[SELECTION]])/this@MU.K[k]     # divided by mu.k
    else if(this@SEC.VAR.EST==5)
      this@SIGMA.K[k]=sqrt(as.numeric(this@SEC.VAR[k]))                                              # from an external file
    else
      cat("Wrong specification for the sector variance!\n")
  }
  
  remove(TEMP)
  remove(SELECTION)

  this@SIGMA2_SYST=sum(this@SIGMA.K^2*this@V.K*this@V.K)  			                                   # portfolio standard diviation
  
  cat("Sector information completed...\n")
  this@SIGMA2_DIV=sum(this@PL^2*this@PD)                                                             # diversifible risk
  cat("Diversifible risk: ",fo(this@SIGMA2_DIV),"  Systematic risk: ",fo(this@SIGMA2_SYST),"\n")
    
  this@EL=sum(this@PL0*this@PD0)                                                                     # expected loss
  this@SD=sqrt(this@SIGMA2_DIV+this@SIGMA2_SYST)                                                     # standard deviation
  
  cat("Calculating portfolio statistics....\n")
  cat("Portfolio net exposure:",fo(sum(as.numeric(this@NEX))),"\n")
  cat("Portfolio potential loss:",fo(sum(as.numeric(this@PL0))),"\n")
  cat("Portfolio expected loss:",fo(this@EL),"\n")
  cat("Portfolio standad deviation:",fo(this@SD),"\n")
  cat("Max. exposure band per CP: ",max(this@NU),"\n",sep="")
  cat("Discretization completed...\n")

  if(this@save.memory){                                                                              # PD0 and PL0 are not needed anymore
    this@PL0=0
    this@PD0=0
  }
  this@changes.crp.calc.portfolio.statistics=FALSE
  gc()
	return(this)
}
)



setGeneric("crp.CSFP.loss",function(this) standardGeneric("crp.CSFP.loss"))
setMethod(f="crp.CSFP.loss",signature=c("crp.CSFP"),definition=function(this){
#
#       <crp.CSFP.loss> Calculation of the portfolio loss distribution 
#                               modified Nested evaluation according to Haaf, Reiﬂ, Schoenmakers (2003)
#                               Chapter 5 in Gundlach/Lehrbass (2003), p.74-75
#       Author:         Dr. Matthias Fischer & Kevin Jakob
#       Last Modified:  07/12/2011
#
  
  if(this@changes.crp.read)
    cat("WARNING: Inputparameter effecting crp.read have been changed, without running crp.read and the following routines afterwards.\n")
  if(this@changes.crp.plausi)
    cat("WARNING: Inputparameter effecting crp.plausi have been changed, without running crp.plausi and the following routines afterwards.\n")
  if(this@changes.crp.calc.portfolio.statistics)
    cat("WARNING: Inputparameter effecting crp.calc.portfolio.statistics have been changed, without running crp.calc.portfolio.statistics and the following routines afterwards.\n")  
  
  if(deparse(substitute(this))!="this")                                                              # synchronize model name to the calling model 
    this@NAME=deparse(substitute(this))                                                              # if it is not called from crp.CSFP() 
  
      
  if(this@NITER.MAX<1){				                                                                       # calculate CDF until ALPHA.MAX level is reached
	  this@M=min(this@NITER.MAX.GLOBAL,sum(this@NU))						                                                                           # constant of maximal number of iterations if
	  this@ALPHA.MAX=this@NITER.MAX			                                                               # loss distribution should be calculated till certain confidenc level.
    cat("Calculate the loss distribution till ",this@NITER.MAX,"-confidence level is reached.",sep="","\n")  # It has no effect if NITER.MAX > 1 is set manuelly.
	}							                                                                                     							
	else{
	  this@M=min(this@NITER.MAX,sum(this@NU))
		this@ALPHA.MAX=1
    cat("Number of probabilities to be calculated (NITER.MAX): ",this@NITER.MAX,sep="","\n")
	} 
  cat("Loss unit: ",fo(this@LOSS.UNIT),"\n",sep="")  
   
      
  A=matrix(0,ncol=(this@M+1),nrow=this@NS)                                                           # Make all needed variables local to this 
  B=A                                                                                                # function instead of going up the environment 
  a=rep(0,this@M+1)                                                                                  # every calling of 'this' . This will increase 
  PDF=rep(0,this@M+1)                                                                                # speed but needs more memory.
  CDF=rep(0,this@M+1)
  SIGMA.K=this@SIGMA.K
  MU.K=this@MU.K
  NU=this@NU
  W=this@W
  PD=this@PD

##############################################################################################################
#       1. Calculation of PDF
##############################################################################################################

        
  if(sum(W[,2:(this@NS+1)])==0){                                                                     # just idiosyncratic
    a[1]=-sum(W[,1]*PD)
    PDF[1]=exp(a[1])

    for(j in 1:this@M){
      SELECT=which(NU==j)
      sum1=0
      if(length(SELECT)>0)
	      sum1=sum(W[SELECT,1]*PD[SELECT])
      a[j+1]=sum1
      PDF[j+1]=sum((1:j)/j*a[2:(j+1)]*PDF[j:1])
    }
  }

  else{                                                                                              # standard case
    for(k in 1:this@NS){
	    A[k,1]=1+SIGMA.K[k]^2*MU.K[k]
	    B[k,1]=-log(A[k,1])
	  }
	  a[1]=-sum(W[,1]*PD)+sum(B[,1]/(SIGMA.K[1:this@NS]^2))
	  PDF[1]=exp(a[1])
	  CDF[1]=PDF[1]
	
	  j=0

	  while(j<this@M && CDF[j+1]<this@ALPHA.MAX){
		  j=j+1
		  if(j%%1e3==0)
  	    cat(" it=",j," CDF=",CDF[j],";")
      SELECT=which(NU==j)
		  for(k in 1:this@NS){
		    if(length(SELECT)>0)
			    A[k,j+1]=SIGMA.K[k]^2*sum(W[SELECT,k+1]*PD[SELECT])
		    if(j+1==2)
			    B[k,2]=A[k,2]/A[k,1]
		    else
			    B[k,j+1]=1/A[k,1]*(A[k,j+1]+(1/j*sum((1:(j-1))*B[k,2:j]*A[k,j:2])))
		  }
		
		  sum1=0
      if(length(SELECT)>0)
        sum1=sum(W[SELECT,1]*PD[SELECT])
      a[j+1]=sum1+sum(B[,j+1]/(SIGMA.K[1:this@NS]^2))
      PDF[j+1]=sum((1:j)/j*a[2:(j+1)]*PDF[j:1])
		  CDF[j+1]=CDF[j]+PDF[j+1]
	  }
    cat("\n")

    remove(list=c("A","SELECT","W","SIGMA.K","MU.K","PD","NU"))	
	  this@PDF=PDF
    remove(PDF)
	  this@CDF=CDF
    remove(CDF)
	  this@a=a
    remove(a)
	  this@B=B
	  remove(B)      
      
  }
      
  length(this@CDF)=j+1                                                                               # cut off the variables to the needed length
  length(this@a)=j+1
	length(this@PDF)=j+1
	this@B=matrix(this@B[,-((j+1):(this@M+1))],nrow=this@NS)

##############################################################################################################
#       3. Summarize and write distribution
##############################################################################################################

  this@LOSS=(0:j)*this@LOSS.UNIT                                                                     # create losses
  this@ALPHA.MAX=this@CDF[length(this@CDF)]
  this@M=j
  cat("Reached level of confidence: ",this@ALPHA.MAX," ( iterations actually done: ",this@M," )\n",sep="")
  cat("Calculation completed...\n")

  cat("Exporting loss distribution...\n")
  FILE.TITLE="LOSSDIST"
  FILE.NAME=paste(this@PATH.OUT,this@NAME,"\\",FILE.TITLE,".csv",sep="")
  dir.create(paste(this@PATH.OUT,this@NAME,"\\",sep=""),showWarnings=FALSE)
  if(this@file.format=="csv")
    write.csv(matrix(cbind(this@LOSS,this@PDF,this@CDF),nrow=this@M+1,ncol=3,dimnames=list(1:length(this@PDF),c("Loss","pdf","cdf"))), file=FILE.NAME,row.names=FALSE)
  else if(this@file.format=="csv2")
    write.csv2(matrix(cbind(this@LOSS,this@PDF,this@CDF),nrow=this@M+1,ncol=3,dimnames=list(1:length(this@PDF),c("Loss","pdf","cdf"))), file=FILE.NAME,row.names=FALSE)
 
  
  
  if(this@save.memory && !this@CALC.RISK.CONT){                                                      # delete data a and B if not needed anymore
    this@B=matrix()
    this@a=0
  }
  if(this@save.memory){                                                                              # don't store LOSS and CDF permanently in 
    this@LOSS=0                                                                                      # save.memory mode
    this@CDF=0
  }

  this@changes.crp.CSFP.loss=FALSE
  gc()
  return(this)        
}
)



setGeneric("crp.measure",function(this) standardGeneric("crp.measure"))
setMethod(f="crp.measure",signature=c("crp.CSFP"),definition=function(this){
#
#       <crp.measure>   Calculate portfolio measures out of the calculated distribution.
#
#       Last Modified:  07/12/2011
#       Author:         Dr. Matthias Fischer, Stefan Kolb & Kevin Jakob
#
#

  if(this@changes.crp.read)
    cat("WARNING: Inputparameter effecting crp.read have been changed, without running crp.read and the following routines afterwards.\n")
  if(this@changes.crp.plausi)
    cat("WARNING: Inputparameter effecting crp.plausi have been changed, without running crp.plausi and the following routines afterwards.\n")
  if(this@changes.crp.calc.portfolio.statistics)
    cat("WARNING: Inputparameter effecting crp.calc.portfolio.statistics have been changed, without running crp.calc.portfolio.statistics and the following routines afterwards.\n")
  if(this@changes.crp.CSFP.loss)
    cat("WARNING: Inputparameter effecting crp.CSFP.loss have been changed, without running crp.CSFP.loss and the following routines afterwards.\n") 
  
  if(length(this@LOSS)==1 && this@save.memory)                                                       # recreate losses if not available
    this@LOSS=this@LOSS.UNIT*(0:this@M)
  if(length(this@CDF)==1 && this@save.memory)                                                        # recalculate CDF if not available
    this@CDF=cumsum(this@PDF)
            
  cat("Calculating portfolio statistics....\n")
        
  this@EL.crp=sum(as.numeric(this@LOSS*this@PDF))                                                    # crp. expected loss
  cat("CR+ portfolio expected loss:",fo(this@EL.crp),"\n")
  cat("Expected loss difference:",fo(this@EL.crp-this@EL),"\n")
  cat("CR+ Exceedance Probability of the expected loss:",1-this@CDF[floor(this@EL/this@LOSS.UNIT)],"\n")

  this@SD.crp=sqrt(sum(as.numeric(this@PDF*(this@LOSS-this@EL.crp)^2)))                              # crp. standard deviation
  cat("CR+ portfolio standard deviation:",fo(this@SD.crp),"\n")  
  
  L.ALPHA=length(this@ALPHA)
  this@VaR=rep(0,L.ALPHA)
  this@EC=rep(0,L.ALPHA)
  this@SELV=rep(0,L.ALPHA)
  
  for(i in 1:L.ALPHA){
	  if(this@ALPHA.MAX>this@ALPHA[i]){					                                                       # check if ALPHA-quantile exists
      this@SELV[i]=min(which(this@CDF>this@ALPHA[i]))                                                # save position of ALPHA-quantile and VaR
      this@VaR[i]=round(this@LOSS[this@SELV[i]],0)
      cat("CR+ portfolio Value-at-risk(",this@ALPHA[i],"): ",fo(this@VaR[i]),sep="","\n")
		}
		else{
		  cat("WARNING: Value-at-risk(",this@ALPHA[i],") is not available, increase LOSS.UNIT or NITER.MAX",sep="","\n")
			this@VaR[i]=0
		}
	}
  this@EC=this@VaR-this@EL.crp
  for(i in 1:L.ALPHA){
    if(this@EC[i]>0)
      cat("CR+ portfolio economic capital(",this@ALPHA[i],"): ",fo(this@EC[i]),sep="","\n")
  }
  for(i in 1:L.ALPHA){
	  if(this@ALPHA.MAX>this@ALPHA[i]){                                                                # check if ALPHA-quantiles exists
      if(this@SELV[i]==1)
        this@ES[i]=this@EL                                                                           # calculate ES
      else
        this@ES[i]=(this@EL-sum(this@LOSS[1:(this@SELV[i]-1)]*this@PDF[1:(this@SELV[i]-1)]))/(1-this@CDF[this@SELV[i]-1])   # calculate ES 
      cat("CR+ portfolio Expected Shortfall(",this@ALPHA[i],"): ",fo(this@ES[i]),sep="","\n")        # "backwards" because of EL/EL.crp difference
		}
	  else{
		  cat("WARNING: Expected Shortfall (",this@ALPHA[i],") is not available, increase LOSS.UNIT or NITER.MAX",sep="","\n")
		  this@SELV[i]=0
		  this@ES[i]=0
		}
	}
  this@ALPHA.crp=this@CDF[this@SELV]                                                                 # save crp. ALPHAs for further calculations
  ES.EL=(this@EL-sum(this@LOSS[1:floor(this@EL/this@LOSS.UNIT)]*this@PDF[1:floor(this@EL/this@LOSS.UNIT)])) /(1-this@CDF[floor(this@EL/this@LOSS.UNIT)])
  cat("CR+ portfolio mean expected loss exceedance: ",fo(ES.EL),sep="","\n")

  this=crp.write.summary(this)                                                                       # write input/output data to file
  if(this@save.memory){
    this@LOSS=0
    this@CDF=0
  }
  this@changes.crp.measure=FALSE 
  gc()
	return(this)
}
)

setGeneric("crp.plot",function(this) standardGeneric("crp.plot"))
setMethod(f="crp.plot",signature=c("crp.CSFP"),definition=function(this){
#
#       <crp.plot>      Plotting the calculated loss distribution together with key numbers from crp.measure
#       Author:         Dr. Matthias Fischer & Kevin Jakob
#       Last Modified:  07/12/2011
#

  if(this@changes.crp.read)
    cat("WARNING: Inputparameter effecting crp.read have been changed, without running crp.read and the following routines afterwards.\n")
  if(this@changes.crp.plausi)
    cat("WARNING: Inputparameter effecting crp.plausi have been changed, without running crp.plausi and the following routines afterwards.\n")
  if(this@changes.crp.calc.portfolio.statistics)
    cat("WARNING: Inputparameter effecting crp.calc.portfolio.statistics have been changed, without running crp.calc.portfolio.statistics and the following routines afterwards.\n")
  if(this@changes.crp.CSFP.loss)
    cat("WARNING: Inputparameter effecting crp.CSFP.loss have been changed, without running crp.CSFP.loss and the following routines afterwards.\n")
  if(this@changes.crp.measure)
    cat("WARNING: Inputparameter effecting crp.measure have been changed, without running crp.measure and the following routines afterwards.\n")
  
  if(deparse(substitute(this))!="this")                                                              # synchronize model name to the calling model
    this@NAME=deparse(substitute(this))                                                              # if it is not called from crp.CSFP()

  if(length(this@LOSS)==1 && this@save.memory)                                                       # recreate losses if not available
    this@LOSS=this@LOSS.UNIT*(0:this@M)
  if(length(this@CDF)==1 && this@save.memory)                                                        # recalculate CDF if not available
    this@CDF=cumsum(this@PDF)

  if(this@PLOT.RANGE.X[1]<1 && this@PLOT.RANGE.X[1]>0)                                          	   # convert plot intervall of confidence levels
	  this@PLOT.RANGE.X[1]=this@LOSS[min(which(this@CDF>this@PLOT.RANGE.X[1]))]	                       # into an intervall of absolute losses
	if(this@PLOT.RANGE.X[2]<1 && this@PLOT.RANGE.X[2]>0)
	  this@PLOT.RANGE.X[2]=this@LOSS[min(which(this@CDF>this@PLOT.RANGE.X[2]))]
	 
  unit=fo(this@PLOT.SCALE)
  PLOT.TITLE.XLAB=paste("Loss in ",unit,sep="")
  PLOT.TITLE.YLAB="Probability"
  PLOT.TITLE=this@NAME
  title=("CSFP-model")
	  	  
  if(all((this@PLOT.RANGE.X==c(0,0) & this@PLOT.RANGE.Y==c(0,0))==TRUE))                             # distinguish if x-/y ranges are given or choosen by R automatically
    plot(this@LOSS/this@PLOT.SCALE,this@PDF,type="l",main=paste("Portfolio Credit Loss (",PLOT.TITLE,")\n",title,sep=""),xlab=PLOT.TITLE.XLAB,ylab=PLOT.TITLE.YLAB,cex.axis=1.3,cex.main=1.3,lwd=3,cex.lab=1.3)
	else if(any((this@PLOT.RANGE.X!=c(0,0))==TRUE))
  	plot(this@LOSS/this@PLOT.SCALE,this@PDF,type="l",main=paste("Portfolio Credit Loss (",PLOT.TITLE,")\n",title,sep=""),xlab=PLOT.TITLE.XLAB,ylab=PLOT.TITLE.YLAB,cex.axis=1.3,cex.main=1.3,lwd=3,cex.lab=1.3,xlim=this@PLOT.RANGE.X/this@PLOT.SCALE)
	else if(any((this@PLOT.RANGE.Y!=c(0,0))==TRUE))
  	plot(this@LOSS/this@PLOT.SCALE,this@PDF,type="l",main=paste("Portfolio Credit Loss (",PLOT.TITLE,")\n",title,sep=""),xlab=PLOT.TITLE.XLAB,ylab=PLOT.TITLE.YLAB,cex.axis=1.3,cex.main=1.3,lwd=3,cex.lab=1.3,ylim=this@PLOT.RANGE.Y)
	else 
  	plot(this@LOSS/this@PLOT.SCALE,this@PDF,type="l",main=paste("Portfolio Credit Loss (",PLOT.TITLE,")\n",title,sep=""),xlab=PLOT.TITLE.XLAB,ylab=PLOT.TITLE.YLAB,cex.axis=1.3,cex.main=1.3,lwd=3,cex.lab=1.3,ylim=this@PLOT.RANGE.Y,xlim=this@PLOT.RANGE.X/this@PLOT.SCALE)

  nVaR=0
  nES=0
  if(this@EL.crp>0)                                                                                  # check if EL, VaR and ES exists
    lines=c(this@EL.crp)
  for(i in 1:length(this@VaR)){
    if(this@VaR[i]>0){
      lines=c(lines,this@VaR[i])
      nVaR=i
    }
  }
  for(i in 1:length(this@ES)){
    if(this@ES[i]>0){
      lines=c(lines,this@ES[i])
      nES=i
    }
  }
  
  abline(v=c(this@EL.crp,this@VaR,this@ES)/this@PLOT.SCALE,col=c("green",rep("blue",nVaR),rep("red",nES)))   # add lines for EL, VaR and ES
  legend("topright",c("EL.crp","VaR","ES"),col=c("green","blue","red"),lwd=1)                        # add legend
  
  if(this@save.memory){                                                                              # dont store LOSS and CDF permanently in
    this@LOSS=0                                                                                      # save.memory mode
    this@CDF=0
  }
  this@changes.crp.plot=FALSE
  gc()
  return(this)
}
)


setGeneric("crp.CSFP.rc.vares",function(this) standardGeneric("crp.CSFP.rc.vares"))
setMethod(f="crp.CSFP.rc.vares",signature=c("crp.CSFP"),definition=function(this){
#
#       <crp.CSFP.rc.vares>    Calculation of contributions to VaR and Expected Shortfall
#
#       Last Modified:  07/12/2011
#       Author:         Dr. Matthias Fischer, Stefan Kolb & Kevin Jakob
#

  if(this@changes.crp.read)
    cat("WARNING: Inputparameter effecting crp.read have been changed, without running crp.read and the following routines afterwards.\n")
  if(this@changes.crp.plausi)
    cat("WARNING: Inputparameter effecting crp.plausi have been changed, without running crp.plausi and the following routines afterwards.\n")
  if(this@changes.crp.calc.portfolio.statistics)
    cat("WARNING: Inputparameter effecting crp.calc.portfolio.statistics have been changed, without running crp.calc.portfolio.statistics and the following routines afterwards.\n")
  if(this@changes.crp.CSFP.loss)
    cat("WARNING: Inputparameter effecting crp.CSFP.loss have been changed, without running crp.CSFP.loss and the following routines afterwards.\n")
  if(this@changes.crp.measure)
    cat("WARNING: Inputparameter effecting crp.measure have been changed, without running crp.measure and the following routines afterwards.\n") 
  
  l=length(this@SELV)
	
  if(!(this@SELV[l]>0 && this@ES[l]>0)){                                                             # check if VaR and ES exists
	  cat("VaR and ES contributions are not available. \n")
		this@rc.OK=FALSE
		return(this)
	}
	if(length(this@a)==0 || all(dim(this@B)==1)){                                                      # check if required data exists
    cat("Necessary data for calculation of risk contributions (VaR/ES) are not availabel, probably you`re running in save.memory mode. \n Change save.memory to FALSE or CALC.RISK.CONT to TRUE, run crp.CSFP.loss and start calculation of risk contributions (VaR/ES) again.\n")
    return(this)
  }
  
  if(l>1)
	  ALPHA=this@ALPHA[l]                                                                              # calculate RC just for last ALPHA
  cat("Calculating VaR and ES contributions....\n")
         
  
  Z=length(this@PDF)                                                                                 # initialization
  VaR.K=numeric(this@NC)
  VaR.K=this@SELV[l]-this@PL/this@LOSS.UNIT
  ALPHA.M=matrix(0,nrow=this@NS+1,ncol=this@SELV[l])
  PLH=matrix(0,nrow=this@NS+1,ncol=Z)
  PLH[1,]=this@PDF
  PDVAR=this@PDF[this@SELV[l]]
    
  VaR.CONT=numeric(this@NC)                                                                          # Make all needed variables local to this
  ES.CONT=numeric(this@NC)                                                                           # function instead of going up the environment
  a=this@a                                                                                           # every calling of 'this' . This will increase
  B=this@B                                                                                           # speed.
  SELV=this@SELV
  PL=this@PL
  PD=this@PD
  NS=this@NS
  W=this@W
  NC=this@NC                                                                                         # probability of the VaR
      
  if (NS==1)
    B=matrix(nrow=1,B)
  a1=a[1]+B[,1]                                                                                      # for details please see SAS documentation or p.87-90 in "Credit Risk in the Banking Industry".
  PLH[2:(NS+1),1]=exp(a1)                                                                            # calculation of the "distorted" PDF for each sector 
  for(h in 2:(NS+1)){
    ALPHA.M[h,]=a[1:SELV[l]]+B[h-1,1:SELV[l]]
    for(j in 2:SELV[l])
      PLH[h,j]=sum((1:(j-1))/(j-1)*ALPHA.M[h,2:j]*PLH[h,(j-1):1])
  }
  CLH=PLH
  for(x in 1:(NS+1))                                                                                 # calculation of the CDF
    CLH[x,]=cumsum(PLH[x,])
  ALPHA=this@ALPHA.crp[l]-this@PDF[this@SELV[l]]
  for(i in 1:NC){                                                                                    # Verify if the exposure is equal or bigger than the VaR
    if(VaR.K[i]<=0){
      VaR.CONT[i]=0
      ES.CONT[i]=(PL[i]*PD[i])/(1-ALPHA)
    }
    else if(VaR.K[i]==1){
      VaR.CONT[i]=(PL[i]*PD[i]*sum(W[i,]*PLH[1:(NS+1),VaR.K[i]]))/PDVAR
      ES.CONT[i]=(PL[i]*PD[i])/(1-ALPHA)
    }
    else{
      VaR.CONT[i]=(PL[i]*PD[i]*sum(W[i,]*PLH[1:(NS+1),VaR.K[i]]))/PDVAR
      ES.CONT[i]=(PL[i]*PD[i]*sum(W[i,]*(1-CLH[1:(NS+1),(VaR.K[i]-1)])))/(1-ALPHA)
    }
  }
      
  ES.alt=this@ES[l]
  VaR=this@VaR[l]
  ES=this@ES[l]
  SELV=this@SELV[l]
  alpha=this@ALPHA.crp[l]-this@PDF[SELV]
  while(VaR<ES){                                                                                     # calculation of expected shortfall TAU
    SELV=SELV-1
    alpha=alpha-this@PDF[SELV]
    ES=(this@EL-sum((0:(SELV-2))*this@LOSS.UNIT*this@PDF[1:(SELV-1)]))/(1-alpha)
  }
        
  cat("CR+ Expected Shortfall TAU(",alpha+this@PDF[SELV],"):",fo(ES),"\n")
        
  VaR.K=SELV-this@PL/this@LOSS.UNIT                                                                  # calculation of the contributions for ES TAU 
  ES.TAU.CONT=ES.CONT                                                                                # (analog to ES)
  for(i in 1:NC){
    if(VaR.K[i]<=1)
       ES.TAU.CONT[i]=(PL[i]*PD[i])/(1)
    else           
    	 ES.TAU.CONT[i]=(PL[i]*PD[i]*sum(W[i,]*(1-CLH[1:(NS+1),(VaR.K[i]-1)])))/(1-alpha)
  }
        
  cat("Scale Factor for TAU",sum(VaR.CONT)/sum(ES.TAU.CONT),"\n")
  ES.TAU.CONT=ES.TAU.CONT*sum(VaR.CONT)/sum(ES.TAU.CONT)  		                                       # upscaling
      
  if(round(sum(VaR.CONT),0)==round(VaR,0))
	   cat("Sum Check VaR: OK\n")
  else
	   cat("Sum Check VaR: NOT OK, Difference:",fo(sum(VaR.CONT)-VaR),"\n")
  if(round(sum(ES.CONT),0)==round(ES.alt,0))
	   cat("Sum Check VaR: OK\n")
  else
	   cat("Sum Check VaR: NOT OK, Difference:",fo(sum(this@ES.CONT)-ES.alt),"\n")
  
  remove(list=c("a","B","SELV","PL","PD","W")) 
  
  this@VaR.CONT=VaR.CONT
  remove(VaR.CONT)
  this@ES.CONT=ES.CONT
  remove(ES.CONT)
  this@ES.TAU.CONT=ES.TAU.CONT
  
  remove(list=c("ES.TAU.CONT","ALPHA.M","PLH","CLH","VaR.K"))
  this@rc.OK=TRUE
	cat("Done....\n")
  this@changes.crp.CSFP.rc.vares=FALSE
  gc()
	return(this)
}
)

setGeneric("crp.CSFP.rc.sd",function(this) standardGeneric("crp.CSFP.rc.sd"))
setMethod(f="crp.CSFP.rc.sd",signature="crp.CSFP",definition=function(this){
#
#       <crp.CSFP.rc.sd>     Calculation of SD Contributions
#
#       Last Modified:  03/11/2011
#       Author:         Dr. Matthias Fischer, Stefan Kolb & Kevin Jakob
#

  if(this@changes.crp.read)
    cat("WARNING: Inputparameter effecting crp.read have been changed, without running crp.read and the following routines afterwards.\n")
  if(this@changes.crp.plausi)
    cat("WARNING: Inputparameter effecting crp.plausi have been changed, without running crp.plausi and the following routines afterwards.\n")
  if(this@changes.crp.calc.portfolio.statistics)
    cat("WARNING: Inputparameter effecting crp.calc.portfolio.statistics have been changed, without running crp.calc.portfolio.statistics and the following routines afterwards.\n")
  if(this@changes.crp.CSFP.loss)
    cat("WARNING: Inputparameter effecting crp.CSFP.loss have been changed, without running crp.CSFP.loss and the following routines afterwards.\n")
  if(this@changes.crp.measure)
    cat("WARNING: Inputparameter effecting crp.measure have been changed, without running crp.measure and the following routines afterwards.\n")   
  
  cat("Calculating SD contributions....\n")

      
  NC=this@NC                                                                                         # Make all needed variables local to this
  NS=this@NS                                                                                         # function instead of going up the environment
  PL=this@PL                                                                                         # every calling of 'this' . This will increase
  PD=this@PD                                                                                         # speed.
  W=this@W
  SD.CONT=as.numeric(this@NC)
  SD=this@SD
  SIGMA.K=this@SIGMA.K
  V.K=this@V.K
    
  if(sum(W[,2:(NS+1)])==0){                                                                          # just ideosyncratic factor exists
    for(i in 1:NC)
      SD.CONT[i]=(PL[i]*PD[i]/SD)*PL[i]
  }
  else{
    for(i in 1:NC){                                                                                  # calculation based on formula (2.27) in 
      k=sum(SIGMA.K^2*W[i,-1]*V.K)                                                                   # Gundlach/Lehrbass (2003)
      SD.CONT[i]=(PL[i]*PD[i]/SD)*(PL[i]+k)
    }
  }
  if(round(sum(SD.CONT),0)==round(SD,0))
	  cat("Sum Check SD: OK\n")
  else
	  cat("Sum Check SD: NOT OK, Difference:",fo(sum(SD.CONT)-SD),"\n")

  remove(list=c("PL","PD","W","SIGMA.K","V.K"))
  this@SD.CONT=SD.CONT
  remove(SD.CONT)    
    
  cat("Done....\n")
  this@changes.crp.CSFP.rc.sd=FALSE
  gc()
  return(this)
}
)


setGeneric("crp.export",function(this) standardGeneric("crp.export"))
setMethod(f="crp.export",signature=c("crp.CSFP"),definition=function(this){
#
#       <crp.export>    Export the calculated Risk Contributions to a CSV file              
#
#       Last Modified:  07/12/2011
#       Author:         Dr. Matthias Fischer, Stefan Kolb & Kevin Jakob
#
#

  if(this@changes.crp.read)
    cat("WARNING: Inputparameter effecting crp.read have been changed, without running crp.read and the following routines afterwards.\n")
  if(this@changes.crp.plausi)
    cat("WARNING: Inputparameter effecting crp.plausi have been changed, without running crp.plausi and the following routines afterwards.\n")
  if(this@changes.crp.calc.portfolio.statistics)
    cat("WARNING: Inputparameter effecting crp.calc.portfolio.statistics have been changed, without running crp.calc.portfolio.statistics and the following routines afterwards.\n")
  if(this@changes.crp.CSFP.loss)
    cat("WARNING: Inputparameter effecting crp.CSFP.loss have been changed, without running crp.CSFP.loss and the following routines afterwards.\n")
  if(this@changes.crp.measure)
    cat("WARNING: Inputparameter effecting crp.measure have been changed, without running crp.measure and the following routines afterwards.\n") 
  if(this@changes.crp.CSFP.rc.vares)
    cat("WARNING: Inputparameter effecting crp.CSFP.rc.vares have been changed, without running crp.CSFP.rc.vares and the following routines afterwards.\n") 
  if(this@changes.crp.CSFP.rc.sd)
    cat("WARNING: Inputparameter effecting crp.CSFP.rc.sd have been changed, without running crp.CSFP.rc.sd and the following routines afterwards.\n")   
  
  if(deparse(substitute(this))!="this")                                                              # synchronize model name to the calling model
    this@NAME=deparse(substitute(this))                                                              # if it is not called from crp.CSFP()

  cat("Exporting risk contributions...\n")
  if(this@NS==1)
    COL.NAMES=c("CP.NR","PL","CP.RATING","Sektor:1","RC_EL","SD","SD/SD_Gesamt","VaR","VaR/VaR_Gesamt","TAU","TAU/(SUMME TAU)","ES","ES/ES_Gesamt")
  else
    COL.NAMES=c("CP.NR","PL","CP.RATING","Sektor:1",as.character(c(2:this@NS)),"RC_EL","SD","SD/SD_Gesamt", "VaR","VaR/VaR_Gesamt","TAU","TAU/(SUMME TAU)","ES","ES/ES_Gesamt")
  RC=matrix(,nrow=this@NC,ncol=12+this@NS,dimnames=list(1:this@NC,COL.NAMES))                                                           # collect risk contribution in one matrix
  RC[,1]=this@CP.NR
  RC[,2]=this@PL
  RC[,3]=this@CP.RATING
  RC[,4:(this@NS+3)]=this@W[,2:(this@NS+1)]
  RC[,this@NS+4]=this@PL*this@PD
  RC[,this@NS+5]=this@SD.CONT
  RC[,this@NS+6]=this@SD.CONT/sum(this@SD.CONT)
  RC[,this@NS+7]=this@VaR.CONT
  RC[,this@NS+8]=this@VaR.CONT/sum(this@VaR.CONT)
  RC[,this@NS+9]=this@ES.TAU.CONT
  RC[,this@NS+10]=this@ES.TAU.CONT/sum(this@ES.TAU.CONT)
  RC[,this@NS+11]=this@ES.CONT
  RC[,this@NS+12]=this@ES.CONT/sum(this@ES.CONT)
  
  
  FILE.TITLE="RC"# File name of loss distribution
  dir.create(paste(this@PATH.OUT,this@NAME,"\\",sep=""),showWarnings=FALSE)
  if(this@file.format=="csv")
    write.csv(RC, file=paste(this@PATH.OUT,this@NAME,"\\",FILE.TITLE,".csv",sep=""),row.names=FALSE)
  else if(this@file.format=="csv2")
    write.csv2(RC, file=paste(this@PATH.OUT,this@NAME,"\\",FILE.TITLE,".csv",sep=""),row.names=FALSE)
  cat("Done....\n")
  remove(RC)
  
  if(this@save.memory){                                                                              # dont store risk contributions after export 
    this@ES.CONT=0                                                                                   # in save.memory mode
    this@VaR.CONT=0
    this@ES.TAU.CONT=0
    this@SD.CONT=0
  }
	this@changes.crp.export=FALSE
  gc()
	return(this)
}
)



setGeneric("crp.CSFP",function(this) standardGeneric("crp.CSFP"))
setMethod(f="crp.CSFP",signature=c("crp.CSFP"),definition=function(this){

  this@NAME=deparse(substitute(this))                                                                # synchronize model name to the calling model

  org.save.memory=this@save.memory                                                                   # save original state of save.memory and 
  this@save.memory=FALSE                                                                             # change to FALSE during calculations in order 
                                                                                                     # to increase performance
  this=crp.read(this)
  if(!this@read.OK)
    return(this)
  this=crp.plausi(this)
  if(!this@plausi.OK)
    return(this)
  this=crp.calc.portfolio.statistics(this)
  this=crp.CSFP.loss(this)
  this=crp.measure(this)
  if(this@PLOT.PDF)
    this=crp.plot(this)
  else
    cat("You dont want to plot the PDF, otherwise set attribute PLOT.PDF=TRUE (default case)!\n")
  if(this@CALC.RISK.CONT){
    this=crp.CSFP.rc.vares(this)
    this=crp.CSFP.rc.sd(this)
    if(this@rc.OK)
      this=crp.export(this)
  }
  else
    cat("You dont want to calculate the Risk Contributions otherwise set attribute CALC.RISK.CONT=TRUE!\n")
  this@save.memory=org.save.memory                                                                   # restore original save.memory state
    
  if(this@save.memory && !this@CALC.RISK.CONT){
    this@B=matrix()
    this@a=0
    this@ES.CONT=0
    this@VaR.CONT=0
    this@ES.TAU.CONT=0
    this@SD.CONT=0
  }
  if(this@save.memory){
    this@LOSS=0
    this@CDF=0
    this@PD0=0
    this@PL0=0
  }
  gc()
  return(this)
}
)


setGeneric("crp.summary",function(this) standardGeneric("crp.summary"))
setMethod(f="crp.summary",signature=c("crp.CSFP"),definition=function(this){
    S=list(NAME=this@NAME,SEC.VAR.EST=this@SEC.VAR.EST,LOSS.UNIT=this@LOSS.UNIT,NITER.MAX=this@NITER.MAX,NC=this@NC,NS=this@NS,EL=this@EL,EL.crp=this@EL.crp,SD=this@SD,SD.crp=this@SD.crp,SIGMA.DIV=this@SIGMA2_DIV,SIGMA.SYST=this@SIGMA2_SYST,ALPHA.MAX=this@ALPHA.MAX,PL=sum(this@PL),ALPHA=this@ALPHA,VaR=this@VaR,EC=this@EC,ES=this@ES)
    return(S)
}
)

setGeneric("crp.write.summary",function(this) standardGeneric("crp.write.summary"))
setMethod(f="crp.write.summary",signature=c("crp.CSFP"),definition=function(this){
    
  if(deparse(substitute(this))!="this")
        this@NAME=deparse(substitute(this))
  dir.create(paste(this@PATH.OUT,this@NAME,"\\",sep=""),showWarnings=FALSE)
  if(this@file.format=="csv")
    write.csv(crp.summary(this),paste(this@PATH.OUT,this@NAME,"\\summary.csv",sep=""),row.names=FALSE)
  else if(this@file.format=="csv2")
    write.csv2(crp.summary(this),paste(this@PATH.OUT,this@NAME,"\\summary.csv",sep=""),row.names=FALSE)

  return(this)
}
)

setGeneric("crp.integrity.check",function(method,this) standardGeneric("crp.integrity.check"))
setMethod(f="crp.integrity.check",signature=c("character","crp.CSFP"),definition=function(method,this){

  if(method=="crp.read"){
    if(this@changes.crp.read)
      cat("WARNING: Inputparameter possibly effecting this variable have been changed without running all necessary routines afterwards.\n ")
  }
  else if(method=="crp.plausi"){
    if(this@changes.crp.read || this@changes.crp.plausi)
      cat("WARNING: Inputparameter possibly effecting this variable have been changed without running all necessary routines afterwards.\n ")
  }
  else if(method=="crp.calc.portfolio.statistics"){
    if(this@changes.crp.read || this@changes.crp.plausi || this@changes.crp.calc.portfolio.statistics)
      cat("WARNING: Inputparameter possibly effecting this variable have been changed without running all necessary routines afterwards.\n ")
  }
  else if(method=="crp.CSFP.loss"){
    if(this@changes.crp.read || this@changes.crp.plausi || this@changes.crp.calc.portfolio.statistics || this@changes.crp.CSFP.loss)
      cat("WARNING: Inputparameter possibly effecting this variable have been changed without running all necessary routines afterwards.\n ")
  }
  else if(method=="crp.measure"){
    if(this@changes.crp.read || this@changes.crp.plausi || this@changes.crp.calc.portfolio.statistics || this@changes.crp.CSFP.loss || this@changes.crp.measure)
      cat("WARNING: Inputparameter possibly effecting this variable have been changed without running all necessary routines afterwards.\n ")
  }
  else if(method=="crp.plot"){
    if(this@changes.crp.read || this@changes.crp.plausi || this@changes.crp.calc.portfolio.statistics || this@changes.crp.CSFP.loss || this@changes.crp.measure || this@changes.crp.plot)
      cat("WARNING: Inputparameter possibly effecting this variable have been changed without running all necessary routines afterwards.\n ")
  }
  else if(method=="crp.CSFP.rc.vares"){
    if(this@changes.crp.read || this@changes.crp.plausi || this@changes.crp.calc.portfolio.statistics || this@changes.crp.CSFP.loss || this@changes.crp.measure || this@changes.crp.CSFP.rc.vares)
      cat("WARNING: Inputparameter possibly effecting this variable have been changed without running all necessary routines afterwards.\n ")
  }
  else if(method=="crp.CSFP.rc.sd"){
    if(this@changes.crp.read || this@changes.crp.plausi || this@changes.crp.calc.portfolio.statistics || this@changes.crp.CSFP.loss || this@changes.crp.measure || this@changes.crp.CSFP.rc.sd)
      cat("WARNING: Inputparameter possibly effecting this variable have been changed without running all necessary routines afterwards.\n ")
  }
  else if(method=="crp.export"){
    if(this@changes.crp.read || this@changes.crp.plausi || this@changes.crp.calc.portfolio.statistics || this@changes.crp.CSFP.loss || this@changes.crp.measure || this@changes.crp.CSFP.rc.vares || this@changes.crp.CSFP.rc.sd || this@changes.crp.export)
      cat("WARNING: Inputparameter possibly effecting this variable have been changed without running all necessary routines afterwards.\n ")
  }
  return()
}
)

setGeneric("crp.set.changes",function(method,this) standardGeneric("crp.set.changes"))
setMethod(f="crp.set.changes",signature=c("character","crp.CSFP"),definition=function(method,this){
  
  if(method=="crp.read"){
    this@changes.crp.read=TRUE
    this@changes.crp.plausi=TRUE
    this@changes.crp.calc.portfolio.statistics=TRUE
    this@changes.crp.CSFP.loss=TRUE
    this@changes.crp.measure=TRUE
    this@changes.crp.plot=TRUE
    if(!(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.CSFP.rc.vares=TRUE
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0))
      this@changes.crp.CSFP.rc.sd=TRUE
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0) || !(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.export=TRUE
  }
  if(method=="crp.plausi"){
    this@changes.crp.plausi=TRUE
    this@changes.crp.calc.portfolio.statistics=TRUE
    this@changes.crp.CSFP.loss=TRUE
    this@changes.crp.measure=TRUE
    this@changes.crp.plot=TRUE
    if(!(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.CSFP.rc.vares=TRUE
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0))
      this@changes.crp.CSFP.rc.sd=TRUE
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0) || !(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.export=TRUE
  }
  if(method=="crp.calc.portfolio.statistics"){
    this@changes.crp.calc.portfolio.statistics=TRUE
    this@changes.crp.CSFP.loss=TRUE
    this@changes.crp.measure=TRUE
    this@changes.crp.plot=TRUE
    if(!(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.CSFP.rc.vares=TRUE
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0))
      this@changes.crp.CSFP.rc.sd=TRUE
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0) || !(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.export=TRUE
  }
  if(method=="crp.CSFP.loss"){
    this@changes.crp.CSFP.loss=TRUE
    this@changes.crp.measure=TRUE
    this@changes.crp.plot=TRUE
    if(!(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.CSFP.rc.vares=TRUE
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0))
      this@changes.crp.CSFP.rc.sd=TRUE
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0) || !(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.export=TRUE
  }
  if(method=="crp.measure"){
    this@changes.crp.measure=TRUE
    this@changes.crp.plot=TRUE
    if(!(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.CSFP.rc.vares=TRUE
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0))
      this@changes.crp.CSFP.rc.sd=TRUE
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0) || !(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.export=TRUE
  }
  if(method=="crp.plot"){
    this@changes.crp.plot=TRUE
  }
  if(method=="crp.CSFP.rc.vares"){
    if(!(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.CSFP.rc.vares=TRUE
    if(!(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.export=TRUE
  }
  if(method=="crp.CSFP.rc.sd"){
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0))
      this@changes.crp.CSFP.rc.sd=TRUE
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0))
      this@changes.crp.export=TRUE
  }
  if(method=="crp.export"){
    if(!(length(this@SD.CONT)==1 && this@SD.CONT[1]==0) || !(length(this@VaR.CONT)==1 && this@VaR.CONT[1]==0))
      this@changes.crp.export=TRUE
  }
  return(this)
})
  

setGeneric("get.PATH.IN",function(this) standardGeneric("get.PATH.IN"))
setMethod(f="get.PATH.IN",signature=c("crp.CSFP"),definition=function(this) this@PATH.IN)
setGeneric("set.PATH.IN<-",function(this,value) standardGeneric("set.PATH.IN<-"))
setReplaceMethod(f="set.PATH.IN",signature=c("crp.CSFP","character"),definition=function(this,value){this@PATH.IN=value;this=crp.set.changes("crp.read",this);this})

setGeneric("get.PATH.OUT",function(this) standardGeneric("get.PATH.OUT"))
setMethod(f="get.PATH.OUT",signature=c("crp.CSFP"),definition=function(this) this@PATH.OUT)
setGeneric("set.PATH.OUT<-",function(this,value) standardGeneric("set.PATH.OUT<-"))
setReplaceMethod(f="set.PATH.OUT",signature=c("crp.CSFP","character"),definition=function(this,value){this@PATH.OUT=value;this=crp.set.changes("crp.CSFP.loss",this);this})

setGeneric("get.PORT.NAME",function(this) standardGeneric("get.PORT.NAME"))
setMethod(f="get.PORT.NAME",signature=c("crp.CSFP"),definition=function(this) this@PORT.NAME)
setGeneric("set.PORT.NAME<-",function(this,value) standardGeneric("set.PORT.NAME<-"))
setReplaceMethod(f="set.PORT.NAME",signature=c("crp.CSFP","character"),definition=function(this,value){this@PORT.NAME=value;this=crp.set.changes("crp.read",this);this})

setGeneric("get.NAME",function(this) standardGeneric("get.NAME"))
setMethod(f="get.NAME",signature=c("crp.CSFP"),definition=function(this) this@NAME)
setGeneric("set.NAME<-",function(this,value) standardGeneric("set.NAME<-"))
setReplaceMethod(f="set.NAME",signature=c("crp.CSFP","character"),definition=function(this,value){this@NAME=value;this=crp.set.changes("crp.read",this);this})

setGeneric("get.RISK.NAME",function(this) standardGeneric("get.RISK.NAME"))
setMethod(f="get.RISK.NAME",signature=c("crp.CSFP"),definition=function(this) this@RISK.NAME)
setGeneric("set.RISK.NAME<-",function(this,value) standardGeneric("set.RISK.NAME<-"))
setReplaceMethod(f="set.RISK.NAME",signature=c("crp.CSFP","character"),definition=function(this,value){this@RISK.NAME=value;this=crp.set.changes("crp.read",this);this})

setGeneric("get.PDVAR.NAME",function(this) standardGeneric("get.PDVAR.NAME"))
setMethod(f="get.PDVAR.NAME",signature=c("crp.CSFP"),definition=function(this) this@PDVAR.NAME)
setGeneric("set.PDVAR.NAME<-",function(this,value) standardGeneric("set.PDVAR.NAME<-"))
setReplaceMethod(f="set.PDVAR.NAME",signature=c("crp.CSFP","character"),definition=function(this,value){this@PDVAR.NAME=value;this=crp.set.changes("crp.read",this);this})

setGeneric("get.SEC.VAR.EST",function(this) standardGeneric("get.SEC.VAR.EST"))
setMethod(f="get.SEC.VAR.EST",signature=c("crp.CSFP"),definition=function(this) this@SEC.VAR.EST)
setGeneric("set.SEC.VAR.EST<-",function(this,value) standardGeneric("set.SEC.VAR.EST<-"))
setReplaceMethod(f="set.SEC.VAR.EST",signature=c("crp.CSFP","numeric"),definition=function(this,value){this@SEC.VAR.EST=value
                                                                                                       if(value==5)
                                                                                                         this=crp.set.changes("crp.read",this)
                                                                                                       else
                                                                                                         this=crp.set.changes("crp.calc.portfolio.statistics",this)
                                                                                                       this})

setGeneric("get.LOSS.UNIT",function(this) standardGeneric("get.LOSS.UNIT"))
setMethod(f="get.LOSS.UNIT",signature=c("crp.CSFP"),definition=function(this) this@LOSS.UNIT)
setGeneric("set.LOSS.UNIT<-",function(this,value) standardGeneric("set.LOSS.UNIT<-"))
setReplaceMethod(f="set.LOSS.UNIT",signature=c("crp.CSFP","numeric"),definition=function(this,value){this@LOSS.UNIT=value;this=crp.set.changes("crp.calc.portfolio.statistics",this);this})

setGeneric("get.NITER.MAX",function(this) standardGeneric("get.NITER.MAX"))
setMethod(f="get.NITER.MAX",signature=c("crp.CSFP"),definition=function(this) this@NITER.MAX)
setGeneric("set.NITER.MAX<-",function(this,value) standardGeneric("set.NITER.MAX<-"))
setReplaceMethod(f="set.NITER.MAX",signature=c("crp.CSFP","numeric"),definition=function(this,value){this@NITER.MAX=value;this=crp.set.changes("crp.CSFP.loss",this);this})

setGeneric("get.ALPHA",function(this) standardGeneric("get.ALPHA"))
setMethod(f="get.ALPHA",signature=c("crp.CSFP"),definition=function(this) this@ALPHA)
setGeneric("set.ALPHA<-",function(this,value) standardGeneric("set.ALPHA<-"))
setReplaceMethod(f="set.ALPHA",signature=c("crp.CSFP","numeric"),definition=function(this,value){this@ALPHA=value;this=crp.set.changes("crp.measure",this);this})

setGeneric("get.PLOT.PDF",function(this) standardGeneric("get.PLOT.PDF"))
setMethod(f="get.PLOT.PDF",signature=c("crp.CSFP"),definition=function(this) this@PLOT.PDF)
setGeneric("set.PLOT.PDF<-",function(this,value) standardGeneric("set.PLOT.PDF<-"))
setReplaceMethod(f="set.PLOT.PDF",signature=c("crp.CSFP","logical"),definition=function(this,value){this@PLOT.PDF=value;this=crp.set.changes("crp.plot",this);this})

setGeneric("get.CALC.RISK.CONT",function(this) standardGeneric("get.CALC.RISK.CONT"))
setMethod(f="get.CALC.RISK.CONT",signature=c("crp.CSFP"),definition=function(this) this@CALC.RISK.CONT)
setGeneric("set.CALC.RISK.CONT<-",function(this,value) standardGeneric("set.CALC.RISK.CONT<-"))
setReplaceMethod(f="set.CALC.RISK.CONT",signature=c("crp.CSFP","logical"),definition=function(this,value){this@CALC.RISK.CONT=value;this=crp.set.changes("crp.CSFP.rc.vares",this);this})

setGeneric("get.save.memory",function(this) standardGeneric("get.save.memory"))
setMethod(f="get.save.memory",signature=c("crp.CSFP"),definition=function(this) this@save.memory)
setGeneric("set.save.memory<-",function(this,value) standardGeneric("set.save.memory<-"))
setReplaceMethod(f="set.save.memory",signature=c("crp.CSFP","logical"),definition=function(this,value){this@save.memory=value;this=crp.set.changes("crp.CSFP.loss",this);this})

setGeneric("get.PLOT.RANGE.X",function(this) standardGeneric("get.PLOT.RANGE.X"))
setMethod(f="get.PLOT.RANGE.X",signature=c("crp.CSFP"),definition=function(this) this@PLOT.RANGE.X)
setGeneric("set.PLOT.RANGE.X<-",function(this,value) standardGeneric("set.PLOT.RANGE.X<-"))
setReplaceMethod(f="set.PLOT.RANGE.X",signature=c("crp.CSFP","numeric"),definition=function(this,value){this@PLOT.RANGE.X=value;this=crp.set.changes("crp.plot",this);this})

setGeneric("get.PLOT.RANGE.Y",function(this) standardGeneric("get.PLOT.RANGE.Y"))
setMethod(f="get.PLOT.RANGE.Y",signature=c("crp.CSFP"),definition=function(this) this@PLOT.RANGE.Y)
setGeneric("set.PLOT.RANGE.Y<-",function(this,value) standardGeneric("set.PLOT.RANGE.Y<-"))
setReplaceMethod(f="set.PLOT.RANGE.Y",signature=c("crp.CSFP","numeric"),definition=function(this,value){this@PLOT.RANGE.Y=value;this=crp.set.changes("crp.plot",this);this})

setGeneric("get.PLOT.SCALE",function(this) standardGeneric("get.PLOT.SCALE"))
setMethod(f="get.PLOT.SCALE",signature=c("crp.CSFP"),definition=function(this) this@PLOT.SCALE)
setGeneric("set.PLOT.SCALE<-",function(this,value) standardGeneric("set.PLOT.SCALE<-"))
setReplaceMethod(f="set.PLOT.SCALE",signature=c("crp.CSFP","numeric"),definition=function(this,value){this@PLOT.SCALE=value;this=crp.set.changes("crp.plot",this);this})

setGeneric("get.file.format",function(this) standardGeneric("get.file.format"))
setMethod(f="get.file.format",signature=c("crp.CSFP"),definition=function(this) this@file.format)
setGeneric("set.file.format<-",function(this,value) standardGeneric("set.file.format<-"))
setReplaceMethod(f="set.file.format",signature=c("crp.CSFP","character"),definition=function(this,value){if(!(value=="csv" || value=="csv2")){
                                                                                                           cat("Wrong specification of file.format. Choose between csv and csv2.\n")
                                                                                                           return(this)
                                                                                                          }
                                                                                                          this@file.format=value;this=crp.set.changes("crp.read",this);this})


setGeneric("get.NS",function(this) standardGeneric("get.NS"))
setMethod(f="get.NS",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.read",this)
  this@NS})

setGeneric("get.NC",function(this) standardGeneric("get.NC"))
setMethod(f="get.NC",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.read",this)
  this@NC})

setGeneric("get.SEC.VAR",function(this) standardGeneric("get.SEC.VAR"))
setMethod(f="get.SEC.VAR",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.read",this)
  this@SEC.VAR})

setGeneric("get.NEX",function(this) standardGeneric("get.NEX"))
setMethod(f="get.NEX",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@NEX})

setGeneric("get.LGD",function(this) standardGeneric("get.LGD"))
setMethod(f="get.LGD",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@LGD})

setGeneric("get.PL0",function(this) standardGeneric("get.PL0"))
setMethod(f="get.PL0",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@PL0})

setGeneric("get.PD0",function(this) standardGeneric("get.PD0"))
setMethod(f="get.PD0",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@PD0})

setGeneric("get.EL",function(this) standardGeneric("get.EL"))
setMethod(f="get.EL",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@EL})

setGeneric("get.NU",function(this) standardGeneric("get.NU"))
setMethod(f="get.NU",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@NU})

setGeneric("get.PL",function(this) standardGeneric("get.PL"))
setMethod(f="get.PL",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@PL})

setGeneric("get.PD",function(this) standardGeneric("get.PD"))
setMethod(f="get.PD",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.read",this)
  this@PD})

setGeneric("get.CP.RATING",function(this) standardGeneric("get.CP.RATING"))
setMethod(f="get.CP.RATING",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.read",this)
  this@CP.RATING})

setGeneric("get.CP.NR",function(this) standardGeneric("get.CP.NR"))
setMethod(f="get.CP.NR",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.read",this)
  this@CP.NR})

setGeneric("get.W",function(this) standardGeneric("get.W"))
setMethod(f="get.W",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.read",this)
  this@W})

setGeneric("get.rating.PD",function(this) standardGeneric("get.rating.PD"))
setMethod(f="get.rating.PD",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.read",this)
  this@rating.PD})

setGeneric("get.rating",function(this) standardGeneric("get.rating"))
setMethod(f="get.rating",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.read",this)
  this@rating})

setGeneric("get.rating.SD",function(this) standardGeneric("get.rating.SD"))
setMethod(f="get.rating.SD",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.read",this)
  this@rating.SD})

setGeneric("get.M",function(this) standardGeneric("get.M"))
setMethod(f="get.M",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.CSFP.loss",this)
  this@M})

setGeneric("get.MU.K",function(this) standardGeneric("get.MU.K"))
setMethod(f="get.MU.K",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@MU.K})

setGeneric("get.V.K",function(this) standardGeneric("get.V.K"))
setMethod(f="get.V.K",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@V.K})

setGeneric("get.SIGMA.K",function(this) standardGeneric("get.SIGMA.K"))
setMethod(f="get.SIGMA.K",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@SIGMA.K})

setGeneric("get.SIGMA2_DIV",function(this) standardGeneric("get.SIGMA2_DIV"))
setMethod(f="get.SIGMA2_DIV",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@SIGMA2_DIV})

setGeneric("get.SIGMA2_SYST",function(this) standardGeneric("get.SIGMA2_SYST"))
setMethod(f="get.SIGMA2_SYST",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@SIGMA2_SYST})

setGeneric("get.SD",function(this) standardGeneric("get.SD"))
setMethod(f="get.SD",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.calc.portfolio.statistics",this)
  this@SD})

setGeneric("get.ALPHA.MAX",function(this) standardGeneric("get.ALPHA.MAX"))
setMethod(f="get.ALPHA.MAX",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.CSFP.loss",this)
  this@ALPHA.MAX})

setGeneric("get.a",function(this) standardGeneric("get.a"))
setMethod(f="get.a",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.CSFP.loss",this)
  this@a})

setGeneric("get.PDF",function(this) standardGeneric("get.PDF"))
setMethod(f="get.PDF",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.CSFP.loss",this)
  this@PDF})

setGeneric("get.CDF",function(this) standardGeneric("get.CDF"))
setMethod(f="get.CDF",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.CSFP.loss",this)
  this@CDF})

setGeneric("get.B",function(this) standardGeneric("get.B"))
setMethod(f="get.B",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.CSFP.loss",this)
  this@B})

setGeneric("get.LOSS",function(this) standardGeneric("get.LOSS"))
setMethod(f="get.LOSS",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.CSFP.loss",this)
  this@LOSS})

setGeneric("get.VaR",function(this) standardGeneric("get.VaR"))
setMethod(f="get.VaR",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.measure",this)
  this@VaR})

setGeneric("get.EC",function(this) standardGeneric("get.EC"))
setMethod(f="get.EC",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.measure",this)
  this@EC})

setGeneric("get.ES",function(this) standardGeneric("get.ES"))
setMethod(f="get.ES",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.measure",this)
  this@ES})

setGeneric("get.VaR.CONT",function(this) standardGeneric("get.VaR.CONT"))
setMethod(f="get.VaR.CONT",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.CSFP.rc.vares",this)
  this@VaR.CONT})

setGeneric("get.ES.CONT",function(this) standardGeneric("get.ES.CONT"))
setMethod(f="get.ES.CONT",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.CSFP.rc.vares",this)
  this@ES.CONT})

setGeneric("get.ES.TAU.CONT",function(this) standardGeneric("get.ES.TAU.CONT"))
setMethod(f="get.ES.TAU.CONT",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.CSFP.rc.vares",this)
  this@ES.TAU.CONT})

setGeneric("get.EL.crp",function(this) standardGeneric("get.EL.crp"))
setMethod(f="get.EL.crp",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.measure",this)
  this@EL.crp})

setGeneric("get.SD.crp",function(this) standardGeneric("get.SD.crp"))
setMethod(f="get.SD.crp",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.measure",this)
  this@SD.crp})

setGeneric("get.SELV",function(this) standardGeneric("get.SELV"))
setMethod(f="get.SELV",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.measure",this)
  this@SELV})

setGeneric("get.ALPHA.crp",function(this) standardGeneric("get.ALPHA.crp"))
setMethod(f="get.ALPHA.crp",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.measure",this)
  this@ALPHA.crp})

setGeneric("get.SD.CONT",function(this) standardGeneric("get.SD.CONT"))
setMethod(f="get.SD.CONT",signature=c("crp.CSFP"),definition=function(this){
  crp.integrity.check("crp.CSFP.rc.sd",this)
  this@SD.CONT})

setGeneric("get.changes.crp.read",function(this) standardGeneric("get.changes.crp.read"))
setMethod(f="get.changes.crp.read",signature=c("crp.CSFP"),definition=function(this){
  this@changes.crp.read
  })

setGeneric("get.changes.crp.plausi",function(this) standardGeneric("get.changes.crp.plausi"))
setMethod(f="get.changes.crp.plausi",signature=c("crp.CSFP"),definition=function(this){
  this@changes.crp.plausi
  })

setGeneric("get.changes.crp.calc.portfolio.statistics",function(this) standardGeneric("get.changes.crp.calc.portfolio.statistics"))
setMethod(f="get.changes.crp.calc.portfolio.statistics",signature=c("crp.CSFP"),definition=function(this){
  this@changes.crp.calc.portfolio.statistics
  })

setGeneric("get.changes.crp.CSFP.loss",function(this) standardGeneric("get.changes.crp.CSFP.loss"))
setMethod(f="get.changes.crp.CSFP.loss",signature=c("crp.CSFP"),definition=function(this){
  this@changes.crp.CSFP.loss
  })

setGeneric("get.changes.crp.measure",function(this) standardGeneric("get.changes.crp.measure"))
setMethod(f="get.changes.crp.measure",signature=c("crp.CSFP"),definition=function(this){
  this@changes.crp.measure
  })

setGeneric("get.changes.crp.plot",function(this) standardGeneric("get.changes.crp.plot"))
setMethod(f="get.changes.crp.plot",signature=c("crp.CSFP"),definition=function(this){
  this@changes.crp.plot
  })

setGeneric("get.changes.crp.CSFP.rc.vares",function(this) standardGeneric("get.changes.crp.CSFP.rc.vares"))
setMethod(f="get.changes.crp.CSFP.rc.vares",signature=c("crp.CSFP"),definition=function(this){
  this@changes.crp.CSFP.rc.vares
  })

setGeneric("get.changes.crp.CSFP.rc.sd",function(this) standardGeneric("get.changes.crp.CSFP.rc.sd"))
setMethod(f="get.changes.crp.CSFP.rc.sd",signature=c("crp.CSFP"),definition=function(this){
  this@changes.crp.CSFP.rc.sd
  })

setGeneric("get.changes.crp.export",function(this) standardGeneric("get.changes.crp.export"))
setMethod(f="get.changes.crp.export",signature=c("crp.CSFP"),definition=function(this){
  this@changes.crp.export
  })





