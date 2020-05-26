



annual_return <- function(data,year){

  New_data <- data[which(index == 1),]
  Return <- exp(sum(New_data[["Returns"]]))-1
  return(Return)
}
Index <- which(year(ACAS_5minute$Time[(O*window+1):length(SPY_5minute$logr)])==2005)

yearlyreturns <- matrix(0,ncol=6,nrow=5)
for (i in 2005:2009) {
  Index <- which(year(ACAS_5minute$Time[(O*window+1):length(SPY_5minute$logr)])==i)
  yearlyreturns[1,i-2004] <- exp(sum(ACAS_5minute$logr[O*window+1+Index],na.rm=TRUE))-1
  yearlyreturns[2,i-2004] <- exp(sum(HBAN_5minute$logr[O*window+1+Index],na.rm=TRUE))-1
  yearlyreturns[3,i-2004] <- exp(sum(SPY_5minute$logr[O*window+1+Index],na.rm=TRUE))-1
  yearlyreturns[4,i-2004] <- exp(sum(log(b[["sd"]][Index]+1),na.rm=TRUE))-1
  yearlyreturns[5,i-2004] <- exp(sum(log(b[["expected.shortfall"]][Index]+1),na.rm=TRUE))-1
}
yearlyreturns[1,6] <- exp(sum(ACAS_5minute$logr[(O*window+1):length(SPY_5minute$logr)],na.rm=TRUE))-1
yearlyreturns[2,6] <- exp(sum(HBAN_5minute$logr[(O*window+1):length(SPY_5minute$logr)],na.rm=TRUE))-1
yearlyreturns[3,6] <- exp(sum(SPY_5minute$logr[(O*window+1):length(SPY_5minute$logr)],na.rm=TRUE))-1
yearlyreturns[4,6] <- exp(sum(log(b[["sd"]]+1),na.rm=TRUE))-1
yearlyreturns[5,6] <- exp(sum(log(b[["expected.shortfall"]]+1),na.rm=TRUE))-1

colnames(yearlyreturns) <- c("ACAS","HBAN","SPY","MVP","CVAR")
library(xtable)

