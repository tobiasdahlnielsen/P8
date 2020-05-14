library(ghyp);library(tictoc);library(tidyverse);library(magrittr);library(lubridate)

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

HBAN <- HBAN %>% arrange(utcsec)
ACAS <- ACAS %>% arrange(utcsec)
SPY <-  SPY %>% arrange(utcsec)


ACAS %<>% logreturns %<>% simreturns
HBAN %<>% logreturns %<>% simreturns
SPY  %<>% logreturns %<>% simreturns


alllogreturns <- cbind(SPY$logr,ACAS$logr,HBAN$logr)
allsimreturns <- cbind(SPY$simr,ACAS$simr,HBAN$simr)

ACAS %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price,logr,simr)
HBAN %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price,logr,simr)
SPY  %<>% mutate(Time = ymd_hms(utcsec), Price = price) %>% select(Time,Price,logr,simr)
