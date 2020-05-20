library(ghyp);library(tictoc);library(tidyverse);library(magrittr);library(lubridate);
setwd("~/P8")
#load("~/P8/cleandata.RData")
load("~/P8/altdata.RData")
## Define time period

ACAS <- ACAS[which(ACAS$Time == ymd_hms("2005-01-03 09:30:00")):which(ACAS$Time == ymd_hms("2009-12-31 16:00:00")),]
HBAN <- HBAN[which(HBAN$Time == ymd_hms("2005-01-03 09:30:00")):which(HBAN$Time == ymd_hms("2009-12-31 16:00:00")),]
SPY  <- SPY[which(SPY$Time   == ymd_hms("2005-01-03 09:30:00")):which(SPY$Time  == ymd_hms("2009-12-31 16:00:00")),]

validation <- function(Data,start.time,Window,Refit,fits,lim=TRUE){

Interval_Matrix = function(data,window,Refit){
  Time <- data[["Time"]] ; data <- data[["logr"]]
  #browser()
  number <- ceiling((length(data)-window)/Refit)
  intervals <- matrix(0,nrow = number,ncol = 2)
  intervals[1,] <- c(1,window)
  for(i in 2:(number)){
    intervals[i,] <- intervals[i-1,] + Refit
  }
  return(intervals)
}

Interval = Interval_Matrix(Data, Window,Refit)

f <- function(x){ghyp::dghyp(x,fits)}
if (lim==TRUE) {
  histogram <- ggplot(Data[Interval[start.time,1]:Interval[start.time,2],],aes(logr)) + 
    geom_histogram(aes(y = stat(density),fill = 1,colour = 1),bins = 100) + 
    stat_function(
      #xlim(c(-0.01,0.01)) +
      fun = f, 
      lwd = 1, colour = 1
    ) + theme(legend.position = "none",legend.title = element_blank() )
  
  
}
else  {
histogram <- ggplot(Data[Interval[start.time,1]:Interval[start.time,2],],aes(logr)) + 
  geom_histogram(aes(y = stat(density),fill = "red",colour = "red"),bins = 100) + 
  stat_function(
    fun = f, 
    lwd = 1, colour = 1
  ) + theme(legend.position = "none",legend.title = element_blank() )
}

quants <- function(p){
  ghyp::qghyp(p,fits)
}

qqplot <- ggplot(Data[Interval[start.time,1]:Interval[start.time,2],], aes(sample = logr)) +
  stat_qq(distribution = quants) + 
  stat_qq_line(distribution = quants)


ggpubr::ggarrange(histogram + ggtitle(NULL), qqplot , 
                  labels = NULL,
                  ncol = 2, nrow = 1)
}

##########################################################################################
#1 minute
O = which(SPY[["Time"]] == SPY[["Time"]][1] + days(1)) - 1
testday <- 250
window <- 21

ACAS_fit = ACAS[(testday*O+1):(testday*O+O*window),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)
HBAN_fit = HBAN[(testday*O+1):(testday*O+O*window),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)
SPY_fit  = SPY [(testday*O+1):(testday*O+O*window),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)

validation(ACAS,testday,window*O,O,ACAS_fit)
validation(HBAN,testday,window*O,O,HBAN_fit)
validation(SPY ,testday,window*O,O,SPY_fit)


x = seq(from = -0.01, to = 0.01, length.out = 1000)
y_ACAS = sapply(x,function(x){ghyp::dghyp(x,ACAS_fit)})
y_HBAN = sapply(x,function(x){ghyp::dghyp(x,HBAN_fit)})
y_SPY = sapply(x,function(x){ghyp::dghyp(x,SPY_fit)})

hist(ACAS$logr,breaks = 1000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_ACAS, col = "red")

hist(HBAN$logr,breaks = 1000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_HBAN, col = "red")

hist(SPY$logr,breaks = 1000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_SPY, col = "red")

ghyp::qqghyp(HBAN_fit,gaussian =F)
ghyp::qqghyp(ACAS_fit,gaussian =F)
ghyp::qqghyp(SPY_fit,gaussian =F)



##############################################################################################
#5 minute test

ACAS_5minute = ACAS %>% filter(minute(Time) %% 5 == 0)
HBAN_5minute = HBAN %>% filter(minute(Time) %% 5 == 0)
SPY_5minute  = SPY  %>% filter(minute(Time) %% 5 == 0)
O = which(SPY_5minute[["Time"]] == SPY_5minute[["Time"]][1] + days(1)) - 1
testday <- 1072
window <- 21

ACAS_5minute_fit = ACAS_5minute[(testday*O+1):(testday*O+O*window),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)
HBAN_5minute_fit = HBAN_5minute[(testday*O+1):(testday*O+O*window),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)
SPY_5minute_fit  = SPY_5minute [(testday*O+1):(testday*O+O*window),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)

validation(ACAS_5minute,testday,window*O,O,ACAS_5minute_fit)
validation(HBAN_5minute,testday,window*O,O,HBAN_5minute_fit)
validation(SPY_5minute,testday,window*O,O,SPY_5minute_fit)


x = seq(from = -0.01, to = 0.01, length.out = 1000)
y_ACAS_5minute = sapply(x,function(x){ghyp::dghyp(x,ACAS_5minute_fit)})
y_HBAN_5minute = sapply(x,function(x){ghyp::dghyp(x,HBAN_5minute_fit)})
y_SPY_5minute  = sapply(x,function(x){ghyp::dghyp(x,SPY_5minute_fit)})

hist(ACAS_5minute$logr,breaks = 1000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_ACAS_5minute, col = "red")

hist(HBAN_5minute$logr,breaks = 1000, xlim = c(-0.01, 0.01),probability = T)
lines(x,y_HBAN_5minute, col = "red")

hist(SPY_5minute$logr,breaks = 1000, xlim = c(-0.005, 0.005),probability = T)
lines(x,y_SPY_5minute, col = "red")

ghyp::qqghyp(HBAN_5minute_fit,gaussian =F)
ghyp::qqghyp(ACAS_5minute_fit,gaussian =F)
ghyp::qqghyp(SPY_5minute_fit,gaussian =F)
