library(ghyp);library(tictoc);library(tidyverse);library(magrittr)

setwd("~/P8")

getdbdata = function(series,path = ""){
  library(dbplyr);library(RSQLite);library(DBI);library(tidyverse)
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



ACAS = getdbdata("ACAS")
HBAN = getdbdata("HBAN")
SPY  = getdbdata("SPY")




ACAS %<>% logreturns %<>% simreturns
HBAN %<>% logreturns %<>% simreturns
SPY  %<>% logreturns %<>% simreturns


alllogreturns <- cbind(SPY$logr,ACAS$logr,HBAN$logr)
allsimreturns <- cbind(SPY$simr,ACAS$simr,HBAN$simr)

ACAS %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price,logr,simr)
HBAN %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price,logr,simr)
SPY  %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price,logr,simr)

O = which(SPY[["Time"]] == SPY[["Time"]][1] + days(1)) - 1

#378
tic()
for (i in 1:378) {
  print(i)
  if (i==1) {
    dist <- fit.NIGmv(alllogreturns[1:O*2,],silent=TRUE)
    optvalues <- portfolio.optimize(dist,risk.measure = "expected.shortfall",type = "minimum.risk",distr = "return",silent = TRUE)
    portreturns <- allsimreturns[1:O*2,] %*% optvalues$opt.weights
  }
  # else if (i==27|i==28|i==32|i==38|i==42|i==83|i==83
  #          |i==126|i==141|i==142|i==145|i==146|i==148|i==149|i==150|i==151) {
  #   print("error")
  # }
  else {
    dist <- fit.NIGmv(alllogreturns[(O*2*(i-1)+1):(O*2*i),],silent=TRUE)
    optvalues <-try(portfolio.optimize(dist,risk.measure = "expected.shortfall",type = "minimum.risk",distr = "return",silent = TRUE),silent=TRUE)
    if (class(optvalues)!="try-error") {
      j=i
    }
    if (class(optvalues)=="try-error") {
      dist <- fit.NIGmv(alllogreturns[(O*2*(j-1)+1):(O*2*j),],silent=TRUE)
      optvalues <-portfolio.optimize(dist,risk.measure = "expected.shortfall",type = "minimum.risk",distr = "return",silent = TRUE)
    }
    portreturns <- c(portreturns, allsimreturns[(O*2*(i-1)+1):(O*2*i),] %*% optvalues$opt.weights)
    }
}
toc()


#756
tic()
for (i in 651:756) {
  print(i)
  if (i==1) {
    dist <- fit.NIGmv(alllogreturns[1:391,],silent=TRUE)
    optvalues <- portfolio.optimize(dist,risk.measure = "expected.shortfall",type = "minimum.risk",distr = "return",silent = TRUE)
    portreturns <- allsimreturns[1:391,] %*% optvalues$opt.weights
  }
  # else if (i==27|i==28|i==32|i==38|i==42|i==83|i==83
  #          |i==126|i==141|i==142|i==145|i==146|i==148|i==149|i==150|i==151) {
  #   print("error")
  # }
  else {
    dist <- fit.NIGmv(alllogreturns[(391*(i-1)+1):(391*i),],silent=TRUE)
    optvalues <-try(portfolio.optimize(dist,risk.measure = "expected.shortfall",type = "minimum.risk",distr = "return",silent = TRUE),silent=TRUE)
    if (class(optvalues)!="try-error") {
      j=i
    }
    if (class(optvalues)=="try-error") {
      dist <- fit.NIGmv(alllogreturns[(391*(j-1)+1):(391*j),],silent=TRUE)
      optvalues <-portfolio.optimize(dist,risk.measure = "expected.shortfall",type = "minimum.risk",distr = "return",silent = TRUE)
    }
    portreturns <- c(portreturns, allsimreturns[(391*(i-1)+1):(391*i),] %*% optvalues$opt.weights)
  }
}
toc()






ACAS %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price,logr,simr)
HBAN %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price,logr,simr)
SPY  %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price,logr,simr)

O = which(SPY[["Time"]] == SPY[["Time"]][1] + days(1)) - 1

ACAS %>% ggplot(aes(x = Time, y = Price)) + geom_line()+ggtitle("ACAS")
HBAN %>% ggplot(aes(x = Time, y = Price)) + geom_line()+ggtitle("HBAN")
SPY  %>% ggplot(aes(x = Time, y = Price)) + geom_line()+ggtitle("SPY")





#Full Sample: 
ACAS_fit = ACAS %>% pull(returns) %>% .[-1] %>% nigFit(startValues = "MoM")
HBAN_fit = HBAN %>% pull(returns) %>% .[-1] %>% nigFit(startValues = "MoM")

x = seq(from = -0.01, to = 0.01, length.out = 1000)

y_ACAS = sapply(x,function(x){dnig(x,param = ACAS_fit$param)})
y_HBAN = sapply(x,function(x){dnig(x,param = HBAN_fit$param)})

hist(ACAS$returns,breaks = 10000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_ACAS, col = "red")

hist(HBAN$returns,breaks = 1000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_HBAN, col = "red")
