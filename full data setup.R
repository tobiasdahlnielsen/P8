library(ghyp);library(tictoc);library(tidyverse);library(magrittr);library(lubridate);library(beepr)
setwd("~/P8")
load("~/P8/altdata.RData");remove(ACAS,HBAN)
list <- list.files(paste0("al data"))
list <- str_remove(list,c(".db"))
list <- list[-which(list=="MTB")];list <- list[-which(list=="ICE")]


getdbdata = function(series,path = ""){
  con = dbConnect(RSQLite::SQLite(), dbname = paste0(path,series,".db"))
  result = tbl(con, series) %>% collect() %>% unique()
  dbDisconnect(con)
  return(result)
}

logreturns = function(df){
  library(magrittr)
  df %<>% mutate(logr= c(0,Price %>% log %>% diff))
  return(df)
}

simreturns =  function(df){
  library(magrittr)
  df %<>% mutate(simr = logr %>% exp - 1 )
  return(df)
}

setwd("~/P8/al data")
stocks <- list()
stocks_5minute <- list()
alllogreturns_5min <- c()
allsimreturns_5min <- c()
periode <- c("2005-01-03 09:30:00","2009-12-31 16:00:00")
for (i in 1:length(list)) {
  if (list[[i]]=="SPY") {
    setwd("~/P8")
    stocks[[i]] <- SPY[,1:2];names(stocks[[i]]) <- c("Time","price")
    stocks[[i]] <- stocks[[i]][which(stocks[[i]]$Time == ymd_hms(periode[1])):which(stocks[[i]]$Time == ymd_hms(periode[2])),]
    stocks_5minute[[i]] = stocks[[i]] %>% filter(minute(Time) %% 5 == 0)
    setwd("~/P8/al data")
  }
  else{
    stocks[[i]] <- select(getdbdata(list[[i]]),"utcsec","price")
    stocks[[i]] %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,price)
    stocks[[i]] <- stocks[[i]][which(stocks[[i]]$Time == ymd_hms(periode[1])):which(stocks[[i]]$Time == ymd_hms(periode[2])),]
  }
    stocks_5minute[[i]] = stocks[[i]] %>% filter(minute(Time) %% 5 == 0)
    stocks[[i]]         %<>% logreturns %<>% simreturns
    stocks_5minute[[i]] %<>% logreturns %<>% simreturns
    alllogreturns_5min <- cbind(alllogreturns_5min,stocks_5minute[[i]]$logr)
    allsimreturns_5min <- cbind(allsimreturns_5min,stocks_5minute[[i]]$simr)
    #assign(list[[i]],stocks[[i]])
    #assign(paste0(list[[i]],"_5minute"),stocks_5minute[[i]])

}


for (j in 1:length(stocks_5minute)) {
  plot(stocks_5minute$ACAS$Time,stocks_5minute[[j]]$simr,type="l",main=list[j])
}

####################################################weights################################################################################
weightreturns <- function(allsimreturns,alllogreturns,window,O,risk=c("expected.shortfall","value.at.risk","sd")){
  portfolioreturns <- list()
  portweights <- list()
  portrisk <- list()
  for (j in 1:length(risk)) {
    for (i in 0:(dim(allsimreturns)[1]/O-(window+1))) {
      print(paste0(j,"    ",i))
      if (i==0) {
        dist <- fit.NIGmv(alllogreturns[1:O*window,],silent=TRUE)
        optvalues <- portfolio.optimize(dist,risk.measure = risk[j],type = "minimum.risk",distr = "return",silent = TRUE)
        portreturns <- allsimreturns[(O*window+1):(O*(window+1)),] %*% optvalues$opt.weights
        allweights <- optvalues$opt.weights
        old.weights <- optvalues$opt.weights
        allrisks <- optvalues$risk
        old.risk <- optvalues$risk
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
            old.risk    <- optvalues$risk
            new.risk    <- optvalues$risk 
          }
          if (class(optvalues)=="try-error") { 
            new.weights <- old.weights
            new.risk <- old.risk
          }
        }
        portreturns <- c(portreturns, allsimreturns[(O*(window+i)+1):(O*(window+i +1)),] %*% new.weights)
        allweights <- rbind(allweights,new.weights)
        allrisks <- c(allrisks,new.risk)
      }
    }
    portfolioreturns[[risk[j]]] <- portreturns
    portweights[[risk[j]]] <- allweights
    portrisk [[risk[j]]] <- allrisks
  }
  output <- list(portfolioreturns,portweights,portrisk)
  print(paste0("finished ",risk[j]))
  return(output)
}
O = which(stocks_5minute[[1]][["Time"]] == stocks_5minute[[1]][["Time"]][1] + days(1)) - 1
window <- 21

tic()
load("~/P8/3stockreturns.RData")
a <- weightreturns(allsimreturns_5min,alllogreturns_5min,window,O,risk=c("expected.shortfall","sd"))
load("~/P8/alldataportreturns.RData")
b <- weightreturns(allsimreturns_5min,alllogreturns_5min,window,O,risk=c("expected.shortfall","sd"))
load("~/P8/adjportreturns.RData")
c <- weightreturns(adjsimreturns_5min,adjlogreturns_5min,window,O,risk=c("expected.shortfall","sd"))
toc()
beep()

colnames(b[[2]]$expected.shortfall) <- unlist(list)
colnames(b[[2]]$sd) <- unlist(list)
names(stocks) <- unlist(list)
names(stocks_5minute) <- unlist(list)
for (i in 1:length(list)) {
  plot(b[[2]]$expected.shortfall[,i],type="l",ylim=c(-0.1,0.1),main=list[i])
  #lines(b[[2]]$value.at.risk[,i],col="blue")
  lines(b[[2]]$sd[,i],col="red")
  abline(1/34,0)
}

plot(exp(cumsum(log(b[[1]]$expected.shortfall+1)))-1,type="l")
lines(exp(cumsum(log(b[[1]]$sd+1)))-1,col="red")


############################################split adjusted#########################################
splitstocks <- c()
for (j in 1:length(stocks_5minute)) {
  if (max(stocks_5minute[[j]]$simr)>2) {
    splitstocks[j] <- list[j]
  }
  if (min(stocks_5minute[[j]]$simr)<(-0.4)) {
    splitstocks[j] <- list[j]
    
  }
}
splitstocks[3] <- "AET";splitstocks <- splitstocks[-which(splitstocks=="STT")]
splitstocks <- splitstocks[-which(is.na(splitstocks))]

splits <- c("2005-03-14 09:30:00","2006-02-21 09:30:00",
            "2009-07-01 09:30:00",
            "2006-04-19 09:30:00",
            "2005-01-18 09:30:00",
            "2006-10-25 09:30:00",
            "2007-06-22 09:30:00",
            "2008-01-02 09:30:00",
            "2006-06-26 09:30:00",
            "2005-05-31 09:30:00",
            "2006-08-14 09:30:00")
splitsf <- c(1/2,1/2,
             20,
             1/2,
             1/2,
             100/207,
             1/2,
             44/100,
             1/2,
             1/2,
             1/2
             )
adjstocks <- stocks
for (i in 1:length(splitstocks)) {
  adjstocks[[splitstocks[[i]]]][1:(which(adjstocks[[splitstocks[[i]]]]$Time == ymd_hms(splits[i]))-1),2] <- 
  adjstocks[[splitstocks[[i]]]][1:(which(adjstocks[[splitstocks[[i]]]]$Time == ymd_hms(splits[i]))-1),2]*splitsf[i]
}
adjstocks[[35]] <- NULL
adjlogreturns_5min <- c()
adjsimreturns_5min <- c()
for (i in 1:length(adjstocks)) {
  adjstocks[[i]] = adjstocks[[i]] %>% filter(minute(Time) %% 5 == 0)
  adjstocks[[i]] %<>% logreturns %<>% simreturns
  adjlogreturns_5min <- cbind(adjlogreturns_5min,adjstocks[[i]]$logr)
  adjsimreturns_5min <- cbind(adjsimreturns_5min,adjstocks[[i]]$simr)
  
}

tic()
d <- weightreturns(adjsimreturns_5min,adjlogreturns_5min,window,O,risk = "expected.shortfall")
toc()
beep()

for (i in 1:length(list)) {
  plot(c[[2]]$expected.shortfall[,i],type="l",ylim=c(-0.1,0.1),main=list[i])
  #lines(b[[2]]$value.at.risk[,i],col="blue")
  #lines(c[[2]]$sd[,i],col="red")
  abline(1/34,0)
  plot(b[[2]]$expected.shortfall[,i],type="l",ylim=c(-0.1,0.1),main=list[i])
  #lines(b[[2]]$value.at.risk[,i],col="blue")
  #lines(b[[2]]$sd[,i],col="red")
  abline(1/34,0)
}

plot(exp(cumsum(log(c[[1]]$expected.shortfall+1)))-1,type="l")
lines(exp(cumsum(log(c[[1]]$sd+1)))-1,col="red")


yearlyreturns <- matrix(0,ncol=6,nrow=5)
for (i in 2005:2009) {
  Index <- which(year(ACAS_5minute$Time[(O*window+1):length(SPY_5minute$logr)])==i)
  yearlyreturns[1,i-2004] <- exp(sum(ACAS_5minute$logr[O*window+1+Index],na.rm=TRUE))-1
  yearlyreturns[2,i-2004] <- exp(sum(HBAN_5minute$logr[O*window+1+Index],na.rm=TRUE))-1
  yearlyreturns[3,i-2004] <- exp(sum(SPY_5minute$logr[O*window+1+Index],na.rm=TRUE))-1
  yearlyreturns[4,i-2004] <- exp(sum(log(c[[1]][["sd"]][Index]+1),na.rm=TRUE))-1
  yearlyreturns[5,i-2004] <- exp(sum(log(c[[1]][["expected.shortfall"]][Index]+1),na.rm=TRUE))-1
}
yearlyreturns[1,6] <- exp(sum(ACAS_5minute$logr[(O*window+1):length(SPY_5minute$logr)],na.rm=TRUE))-1
yearlyreturns[2,6] <- exp(sum(HBAN_5minute$logr[(O*window+1):length(SPY_5minute$logr)],na.rm=TRUE))-1
yearlyreturns[3,6] <- exp(sum(SPY_5minute$logr[(O*window+1):length(SPY_5minute$logr)],na.rm=TRUE))-1
yearlyreturns[4,6] <- exp(sum(log(c[[1]][["sd"]]+1),na.rm=TRUE))-1
yearlyreturns[5,6] <- exp(sum(log(c[[1]][["expected.shortfall"]]+1),na.rm=TRUE))-1

colnames(yearlyreturns) <- c("ACAS","HBAN","SPY","MVP","CVAR")
library(xtable)



colnames(adjsimreturns_5min) <- colnames(adjlogreturns_5min) <- list

selected <- sort(sample(1:34,10))

tic()
d <- weightreturns(adjsimreturns_5min[,selected],adjlogreturns_5min[,selected],window,O,risk = c("expected.shortfall","sd"))
toc()
beep()

yearlyreturns <- matrix(0,ncol=6,nrow=5)
for (i in 2005:2009) {
  Index <- which(year(ACAS_5minute$Time[(O*window+1):length(SPY_5minute$logr)])==i)
  yearlyreturns[1,i-2004] <- exp(sum(ACAS_5minute$logr[O*window+1+Index],na.rm=TRUE))-1
  yearlyreturns[2,i-2004] <- exp(sum(HBAN_5minute$logr[O*window+1+Index],na.rm=TRUE))-1
  yearlyreturns[3,i-2004] <- exp(sum(SPY_5minute$logr[O*window+1+Index],na.rm=TRUE))-1
  yearlyreturns[4,i-2004] <- exp(sum(log(c[[1]][["sd"]][Index]+1),na.rm=TRUE))-1
  yearlyreturns[5,i-2004] <- exp(sum(log(c[[1]][["expected.shortfall"]][Index]+1),na.rm=TRUE))-1
}
yearlyreturns[1,6] <- exp(sum(ACAS_5minute$logr[(O*window+1):length(SPY_5minute$logr)],na.rm=TRUE))-1
yearlyreturns[2,6] <- exp(sum(HBAN_5minute$logr[(O*window+1):length(SPY_5minute$logr)],na.rm=TRUE))-1
yearlyreturns[3,6] <- exp(sum(SPY_5minute$logr[(O*window+1):length(SPY_5minute$logr)],na.rm=TRUE))-1
yearlyreturns[4,6] <- exp(sum(log(d[[1]][["sd"]]+1),na.rm=TRUE))-1
yearlyreturns[5,6] <- exp(sum(log(d[[1]][["expected.shortfall"]]+1),na.rm=TRUE))-1

for (i in 1:length(selected)) {
  #plot(d[[2]]$expected.shortfall[,i],type="l",main=list[selected[i]])
  #lines(b[[2]]$value.at.risk[,i],col="blue")
  #lines(c[[2]]$sd[,i],col="red")
  plot(stocks_5minute[[list[selected[i]]]]$price,type="l",main=list[selected[i]])
}

plot(exp(cumsum(log(d[[1]]$expected.shortfall+1)))-1,type="l")
lines(exp(cumsum(log(d[[1]]$sd+1)))-1,col="red")
