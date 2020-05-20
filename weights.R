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

O = which(SPY[["Time"]] == SPY[["Time"]][1] + days(1)) - 1
window <- 21
weightreturns <- function(allsimreturns,alllogreturns,window,O,risk=c("expected.shortfall","value.at.risk","sd")){
  portfolioreturns <- list()
for (j in 1:length(risk)) {
  for (i in 0:(dim(allsimreturns)[1]/O-(window+1))) {
    if (i==0) {
     dist <- fit.NIGmv(alllogreturns[1:O*window,],silent=TRUE)
      optvalues <- portfolio.optimize(dist,risk.measure = risk[j],type = "minimum.risk",distr = "return",silent = TRUE)
      portreturns <- allsimreturns[(O*window+1):(O*(window+1)),] %*% optvalues$opt.weights
   }
   else {
     dist <- fit.NIGmv(alllogreturns[(O*i+1):(O*(window+i)),],silent=TRUE)
      if (dist@converged==TRUE) {
        old.dist <- dist
        new.dist <- dist
      }
      if (dist@converged==FALSE) {
        new.dist <- old.dist
      }
      optvalues <-try(portfolio.optimize(new.dist,risk.measure = "expected.shortfall",type = "minimum.risk",distr = "return",silent = TRUE),silent=TRUE)
      if (class(optvalues)!="try-error") {
        old.weights <- optvalues$opt.weights
        new.weights <- optvalues$opt.weights
      }
      if (class(optvalues)=="try-error") { 
        new.weights <- old.weights
      }
      portreturns <- c(portreturns, allsimreturns[(O*(window+i)+1):(O*(window+i +1)),] %*% new.weights)
    }
  }
  portfolioreturns[[risk[j]]] <- portreturns
}
  print(paste0("finished",risk[j]))
  return(portfolioreturns)
}

a <- weightreturns(allsimreturns,alllogreturns,window,O)
