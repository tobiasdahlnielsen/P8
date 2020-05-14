library(ghyp);library(tictoc);library(tidyverse);library(magrittr);library(lubridate);library(GeneralizedHyperbolic)
setwd("~/P8")
load("~/P8/cleandata.RData")



O = which(SPY[["Time"]] == SPY[["Time"]][1] + days(1)) - 1
window <- 21
tic()
for (i in 0:(756-(window+1))) {
  if (i==0) {
    dist <- fit.NIGmv(alllogreturns[1:O*window,],silent=TRUE)
    optvalues <- portfolio.optimize(dist,risk.measure = "expected.shortfall",type = "minimum.risk",distr = "return",silent = TRUE)
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
    print(i)
    portreturns <- c(portreturns, allsimreturns[(O*(window+i)+1):(O*(window+i +1)),] %*% new.weights)
  }
}
toc()


