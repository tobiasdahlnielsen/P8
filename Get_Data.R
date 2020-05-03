library(ghyp)
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
  df %<>% mutate(simpler = logr %>% exp + 1 )
  return(df)
}



ACAS = getdbdata("ACAS")
HBAN = getdbdata("HBAN")
SPY  = getdbdata("SPY")


library(tidyverse);library(magrittr)


ACAS %<>% logreturns %<>% simreturns
HBAN %<>% logreturns# %<>% simreturns
SPY  %<>% logreturns# %<>% simreturns


allreturns <- cbind(SPY$returns,ACAS$returns,HBAN$returns)

tic()
fittetdist <- fit.NIGmv(allreturns[1:391,],silent=TRUE)
toc()

dghyp(rep(0.95,3), object = fittetdist)

ESghyp(0.05, object = fittetdist, distr = c("return", "loss"))

optvalues <- portfolio.optimize(fittetdist,risk.measure = "expected.shortfall",type = "minimum.risk",distr = "return")

ACAS %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price)
HBAN %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price)
SPY  %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price)

O = which(SPY[["Time"]] == SPY[["Time"]][1] + days(1)) - 1

ACAS %>% ggplot(aes(x = Time, y = Price)) + geom_line()
HBAN %>% ggplot(aes(x = Time, y = Price)) + geom_line()
SPY  %>% ggplot(aes(x = Time, y = Price)) + geom_line()




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
