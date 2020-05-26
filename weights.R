library(ghyp);library(tictoc);library(tidyverse);library(magrittr);library(lubridate)
setwd("~/P8")
#load("~/P8/cleandata.RData")
load("~/P8/altdata.RData")
## Define time period

ACAS <- ACAS[which(ACAS$Time == ymd_hms("2005-01-03 09:30:00")):which(ACAS$Time == ymd_hms("2009-12-31 16:00:00")),]
HBAN <- HBAN[which(HBAN$Time == ymd_hms("2005-01-03 09:30:00")):which(HBAN$Time == ymd_hms("2009-12-31 16:00:00")),]
SPY  <- SPY[which(SPY$Time   == ymd_hms("2005-01-03 09:30:00")):which(SPY$Time  == ymd_hms("2009-12-31 16:00:00")),]

alllogreturns <- cbind(SPY$logr,ACAS$logr,HBAN$logr)
allsimreturns <- cbind(SPY$simr,ACAS$simr,HBAN$simr)

alllogreturns_5min <- cbind(SPY_5minute$logr,ACAS_5minute$logr,HBAN_5minute$logr)
allsimreturns_5min <- cbind(SPY_5minute$simr,ACAS_5minute$simr,HBAN_5minute$simr)

O = which(SPY_5minute[["Time"]] == SPY_5minute[["Time"]][1] + days(1)) - 1
window <- 21

weightreturns <- function(allsimreturns,alllogreturns,window,O,risk=c("expected.shortfall","value.at.risk","sd")){
  portfolioreturns <- list()
  portweights <- list()
for (j in 1:length(risk)) {
  for (i in 0:(dim(allsimreturns)[1]/O-(window+1))) {
    if (i==0) {
     dist <- fit.NIGmv(alllogreturns[1:O*window,],silent=TRUE)
      optvalues <- portfolio.optimize(dist,risk.measure = risk[j],type = "minimum.risk",distr = "return",silent = TRUE)
      portreturns <- allsimreturns[(O*window+1):(O*(window+1)),] %*% optvalues$opt.weights
      allweights <- optvalues$opt.weights
   }
   else {
     dist <- fit.NIGmv(alllogreturns[(O*i+1):(O*(window+i)),],silent=TRUE)
      if (dist@converged==FALSE) {
      new.weights <- old.weights
      }
     else {
      optvalues <-try(portfolio.optimize(dist,risk.measure = risk[j],type = "minimum.risk",distr = "return",silent = TRUE),silent=TRUE)
      
      if (class(optvalues)!="try-error") {
        old.weights <- optvalues$opt.weights
        new.weights <- optvalues$opt.weights
      }
      if (class(optvalues)=="try-error") { 
        new.weights <- old.weights
      }
     }
      portreturns <- c(portreturns, allsimreturns[(O*(window+i)+1):(O*(window+i +1)),] %*% new.weights)
      allweights <- rbind(allweights,new.weights)
    }
  }
  portfolioreturns[[risk[j]]] <- portreturns
  portweights[[risk[j]]] <- allweights
}
  output <- list(portfolioreturns,portweights)
  print(paste0("finished ",risk[j]))
  return(output)
}

a <- weightreturns(allsimreturns,alllogreturns,window,O)
b <- weightreturns(allsimreturns_5min,alllogreturns_5min,window,O)





periode <- SPY_5minute$Time[(O*window+1):length(SPY_5minute$logr)]
plot(x=periode,y=cumsum(log(b[["sd"]]+1)),type="l",ylim=c(-1,0.45),xlab="Time",ylab="Cumulated simple returns",col=1)
lines(periode,exp(cumsum(SPY_5minute$logr[(O*window+1):length(SPY_5minute$logr)]))-1,col="purple")
lines(periode,exp(cumsum(ACAS_5minute$logr[(O*window+1):length(SPY_5minute$logr)]))-1,col="orange")
lines(periode,exp(cumsum(HBAN_5minute$logr[(O*window+1):length(SPY_5minute$logr)]))-1,col="green")
lines(periode,exp(cumsum(log(b[["expected.shortfall"]]+1)))-1,col="blue")
legend("bottomleft",legend = c("MVP","SPY","ACAS","HBAN","CVAR"),col = c("black","purple","orange","green","blue"),lty=1)

plot(x=periode,y=exp(cumsum(log(b[["expected.shortfall"]]+1)))-1,type="l",xlab="Time",ylab="Cumulated simple returns",col=1)



plot(periode,pmax(exp(cumsum(log(b[["expected.shortfall"]]+1)))-
                    exp(cumsum(SPY_5minute$logr[(O*window+1):length(SPY_5minute$logr)])),0),
     type="l",ylim=c(-0.15,0.1),ylab="logreturns",xlab="Time")
lines(periode,pmin(exp(cumsum(log(b[["expected.shortfall"]]+1)))-exp(cumsum(SPY_5minute$logr[(O*window+1):length(SPY_5minute$logr)])),0),col="red")
abline(0,0)

periode2 <- SPY_5minute$Time
plot(periode2,SPY_5minute$Price,type="l",ylab="Price",xlab="Time")
plot(periode2,ACAS_5minute$Price,type="l",ylab="Price",xlab="Time")
plot(periode2,HBAN_5minute$Price,type="l",ylab="Price",xlab="Time")
