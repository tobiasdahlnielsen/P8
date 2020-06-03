library(ghyp);library(tictoc);library(tidyverse);library(magrittr);library(lubridate);
setwd("~/P8")
load("~/P8/altdata.RData");remove(ACAS,HBAN)
list <- list.files(paste0("al data"))
list <- str_remove(list,c(".db"))
list <- list[-which(list=="MTB")]


getdbdata = function(series,path = ""){
  con = dbConnect(RSQLite::SQLite(), dbname = paste0(path,series,".db"))
  result = tbl(con, series) %>% collect() %>% unique()
  dbDisconnect(con)
  return(result)
}

logreturns = function(df){
  library(magrittr)
  df %<>% mutate(logr= c(0,price %>% log %>% diff))
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
  if (i==27) {
    setwd("~/P8")
    stocks[[i]] <- SPY
    stocks[[i]] <- stocks[[i]][which(stocks[[i]]$Time == ymd_hms(periode[1])):which(stocks[[i]]$Time == ymd_hms(periode[2])),]
    stocks_5minute[[i]] = stocks[[i]] %>% filter(minute(Time) %% 5 == 0)
    setwd("~/P8/al data")
  }
  else{
    stocks[[i]] <- select(getdbdata(list[[i]]),"utcsec","price")
    stocks[[i]] %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,price)
    stocks[[i]] <- stocks[[i]][which(stocks[[i]]$Time == ymd_hms(periode[1])):which(stocks[[i]]$Time == ymd_hms(periode[2])),]
    stocks_5minute[[i]] = stocks[[i]] %>% filter(minute(Time) %% 5 == 0)
  }
    stocks[[i]]         %<>% logreturns %<>% simreturns
    stocks_5minute[[i]] %<>% logreturns %<>% simreturns
    alllogreturns_5min <- cbind(alllogreturns_5min,stocks_5minute[[i]]$logr)
    allsimreturns_5min <- cbind(allsimreturns_5min,stocks_5minute[[i]]$simr)
    #assign(list[[i]],test[[i]])
    #assign(paste0(list[[i]],"_5minute"))

}
names(stocks) <- unlist(list)



####################################################weights################################################################################
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
O = which(stocks_5minute[[1]][["Time"]] == stocks_5minute[[1]][["Time"]][1] + days(1)) - 1
window <- 21


b <- weightreturns(allsimreturns_5min,alllogreturns_5min,window,O)

