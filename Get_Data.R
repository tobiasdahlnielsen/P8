library(ghyp)
setwd("~/P8")

getdbdata = function(series,path = ""){
  library(dbplyr);library(RSQLite);library(DBI);library(tidyverse)
  con = dbConnect(RSQLite::SQLite(), dbname = paste0(path,series,".db"))
  result = tbl(con, series) %>% collect() %>% unique()
  dbDisconnect(con)
  return(result)
}

returns = function(df){
  library(magrittr)
  df %<>% mutate(returns = c(0,price %>% log %>% diff))
  return(df)
}



ACAS = getdbdata("ACAS")
HBAN = getdbdata("HBAN")
SPY  = getdbdata("SPY")


library(tidyverse);library(magrittr)


ACAS %<>% returns
HBAN %<>% returns
SPY  %<>% returns

allreturns <- cbind(SPY$returns,ACAS$returns,HBAN$returns)
fittetdist <- fit.NIGmv(allreturns,silent=TRUE)


dghyp(rep(0.95,3), object = fittetdist)

ESghyp(0.05, object = fittetdist, distr = c("return", "loss"))

optvalues <- portfolio.optimize(fittetdist,risk.measure = "expected.shortfall",type = "minimum.risk",distr = "return")


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
