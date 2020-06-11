library(ghyp);library(tictoc);library(tidyverse);library(magrittr);library(lubridate);library(beepr)
setwd("~/P8")
load("~/P8/riskcalc.RData")

tic()
a <- weightreturns(threestocksimreturns,threestocklogreturns,window,O,risk=c("expected.shortfall","sd"))
#b <- weightreturns(allstockssimreturns,allstockslogreturns,window,O,risk=c("expected.shortfall","sd"))
c <- weightreturns(adjsimreturns_5min,adjlogreturns_5min,window,O,risk=c("expected.shortfall","sd"))
toc()
beep()


plot(wperiode,a[[3]]$expected.shortfall,type="l",ylim=c(0,max(c[[3]]$expected.shortfall)),ylab="Risk",xlab="Time",main="CVAR")
lines(wperiode,c[[3]]$expected.shortfall,col="red")
legend("topleft",legend = c("3 stocks","34 stocks"),col = c("black","red"),lty=1)

plot(wperiode,a[[3]]$sd,type="l",ylim=c(0,max(a[[3]]$sd)),ylab="Risk",xlab="Time",main="MVP")
lines(wperiode,c[[3]]$sd,col="red")
legend("topleft",legend = c("3 stocks","34 stocks"),col = c("black","red"),lty=1)

