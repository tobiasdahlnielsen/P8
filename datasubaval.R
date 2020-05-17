library(ghyp);library(tictoc);library(tidyverse);library(magrittr);library(lubridate);library(GeneralizedHyperbolic)
setwd("~/P8")
load("~/P8/cleandata.RData")


HBAN_fit = HBAN %>% pull(logr) %>% .[-1] %>% nigFit(startValues = "MoM")
ACAS_fit = ACAS %>% pull(logr) %>% .[-1] %>% nigFit(startValues = "MoM")
SPY_fit  = SPY  %>% pull(logr) %>% .[-1] %>% nigFit(startValues = "MoM")

HBAN_fit = HBAN %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)
ACAS_fit = ACAS %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)
SPY_fit  = SPY  %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)

ghyp::qqghyp(HBAN_fit,gaussian =F)
ghyp::qqghyp(ACAS_fit,gaussian =F)
ghyp::qqghyp(SPY_fit,gaussian =F)

x = seq(from = -0.01, to = 0.01, length.out = 1000)
y_ACAS = sapply(x,function(x){dnig(x,param = ACAS_fit$param)})
y_HBAN = sapply(x,function(x){dnig(x,param = HBAN_fit$param)})
y_SPY  = sapply(x,function(x){dnig(x,param = SPY_fit$param)})

hist(ACAS$logr,breaks = 1000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_ACAS, col = "red")

hist(HBAN$logr,breaks = 1000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_HBAN, col = "red")

hist(SPY$logr,breaks = 1000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_SPY, col = "red")

#5 minute test
#Subset Every 5th minute 
ACAS_5minute = ACAS %>% filter(minute(Time) %% 5 == 0)
HBAN_5minute = HBAN %>% filter(minute(Time) %% 5 == 0)
SPY_5minute  = SPY  %>% filter(minute(Time) %% 5 == 0)
O = which(SPY_5minute[["Time"]] == SPY_5minute[["Time"]][1] + days(1)) - 1

ACAS_5minute_fit = ACAS_5minute[1:(O*500),] %>% pull(logr) %>% .[-1] %>% nigFit(startValues = "MoM")
HBAN_5minute_fit = HBAN_5minute %>% pull(logr) %>% .[-1] %>% nigFit(startValues = "MoM")
SPY_5minute_fit = SPY_5minute %>% pull(logr) %>% .[-1] %>% nigFit(startValues = "MoM")

ACAS_5minute_fit = ACAS_5minute %>% pull(logr) %>% 
  .[-1] %>% fit.NIGuv(silent=T,alpha.bar=0.01,opt.pars = c(alpha.bar=F))
HBAN_5minute_fit = HBAN_5minute[1:(O*500),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)
SPY_5minute_fit = SPY_5minute[1:(O*500),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)

ghyp::qqghyp(HBAN_5minute_fit,gaussian =F)
ghyp::qqghyp(ACAS_5minute_fit,gaussian =F)
ghyp::qqghyp(SPY_5minute_fit,gaussian =F)



x = seq(from = -0.01, to = 0.01, length.out = 1000)
y_ACAS_5minute = sapply(x,function(x){dnig(x,param = ACAS_5minute_fit$param)})
y_HBAN_5minute = sapply(x,function(x){dnig(x,param = HBAN_5minute_fit$param)})
y_SPY_5minute  = sapply(x,function(x){dnig(x,param = SPY_5minute_fit$param)})

hist(ACAS_5minute$logr,breaks = 10000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_ACAS_5minute, col = "red")

hist(HBAN_5minute$logr,breaks = 1000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_HBAN_5minute, col = "red")

hist(SPY_5minute$logr,breaks = 1000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_SPY_5minute, col = "red")

