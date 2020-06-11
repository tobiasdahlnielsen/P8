library(ghyp);library(tictoc);library(tidyverse);library(magrittr);library(lubridate);
setwd("~/P8")
#load("~/P8/cleandata.RData")
load("~/P8/altdata.RData")
## Define time period

ACAS <- ACAS[which(ACAS$Time == ymd_hms("2005-01-03 09:30:00")):which(ACAS$Time == ymd_hms("2009-12-31 16:00:00")),]
HBAN <- HBAN[which(HBAN$Time == ymd_hms("2005-01-03 09:30:00")):which(HBAN$Time == ymd_hms("2009-12-31 16:00:00")),]
SPY  <- SPY[which(SPY$Time   == ymd_hms("2005-01-03 09:30:00")):which(SPY$Time  == ymd_hms("2009-12-31 16:00:00")),]

validation <- function(Data,start.time,Window,Refit,fits,breaks=100,lim=TRUE){

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
    geom_histogram(aes(y = stat(density),fill = 1,colour = 1),bins = breaks) + 
    xlim(c(-0.01,0.01)) +
    labs(x = "Logreturns")+
    stat_function(
      fun = f, 
      lwd = 1, colour = 1
    ) + theme(legend.position = "none",legend.title = element_blank(),
              panel.background=element_blank(),
              axis.line.x.bottom = element_line(color = 'black'),
              axis.line.y.left   = element_line(color = 'black'))
  
}
else  {
histogram <- ggplot(Data[Interval[start.time,1]:Interval[start.time,2],],aes(logr)) + 
  geom_histogram(aes(y = stat(density),fill = 1,colour = 1),bins = breaks) + 
  labs(x = "Logreturns")+
  stat_function(
    fun = f, 
    lwd = 1, colour = 1
  ) + theme(legend.position = "none",legend.title = element_blank(),
            panel.background=element_blank(),
            axis.line.x.bottom = element_line(color = 'black'),
            axis.line.y.left   = element_line(color = 'black'))
}

quants <- function(p){
  ghyp::qghyp(p,fits)
}

qqplot <- ggplot(Data[Interval[start.time,1]:Interval[start.time,2],], aes(sample = logr)) +
  stat_qq(distribution = quants) + 
  stat_qq_line(distribution = quants)+
  theme(panel.background=element_blank(),
        axis.line.x.bottom = element_line(color = 'black'),
        axis.line.y.left   = element_line(color = 'black'))


ggpubr::ggarrange(histogram + ggtitle(NULL), qqplot , 
                  labels = NULL,
                  ncol = 2, nrow = 1)
}

##########################################################################################
#1 minute
O = which(SPY[["Time"]] == SPY[["Time"]][1] + days(1)) - 1
testday <- 505
window <- 21


ACAS_fit = ACAS[(testday*O+1):(testday*O+O*window),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)
HBAN_fit = HBAN[(testday*O+1):(testday*O+O*window),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)
SPY_fit  = SPY [(testday*O+1):(testday*O+O*window),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)


validation(ACAS,testday,window*O,O,ACAS_fit,lim=FALSE)
validation(HBAN,testday,window*O,O,HBAN_fit,lim=FALSE)
validation(SPY ,testday,window*O,O,SPY_fit,lim=FALSE)



##############################################################################################
#5 minute

ACAS_5minute = ACAS[,1:2] %>% filter(minute(Time) %% 5 == 0)
HBAN_5minute = HBAN[,1:2] %>% filter(minute(Time) %% 5 == 0)
SPY_5minute  = SPY[,1:2]  %>% filter(minute(Time) %% 5 == 0)

ACAS_5minute %<>% logreturns %<>% simreturns
HBAN_5minute %<>% logreturns %<>% simreturns
SPY_5minute  %<>% logreturns %<>% simreturns

O = which(SPY_5minute[["Time"]] == SPY_5minute[["Time"]][1] + days(1)) - 1
testday <- 965
window <- 21

ACAS_5minute_fit = ACAS_5minute[(testday*O+1):(testday*O+O*window),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)
HBAN_5minute_fit = HBAN_5minute[(testday*O+1):(testday*O+O*window),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)
SPY_5minute_fit  = SPY_5minute [(testday*O+1):(testday*O+O*window),] %>% pull(logr) %>% .[-1] %>% fit.NIGuv(silent=T)

validation(ACAS_5minute,testday,window*O,O,ACAS_5minute_fit,lim=F)
validation(HBAN_5minute,testday,window*O,O,HBAN_5minute_fit,lim=F)
validation(SPY_5minute,testday,window*O,O,SPY_5minute_fit,lim=F)


