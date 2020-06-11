library(ghyp);library(tictoc);library(tidyverse);library(magrittr);library(lubridate)

setwd("~/P8")

load("~/P8/altdata.RData")
library(readr)

HBAN_20070103 <- read_csv("HBAN_20070103.csv")

data1 <- HBAN_20070103
data1 <- data1[,c(1,3)]
#collect <- tbl(DB, "WFC") %>%  collect() %>% arrange(utcsec)
data2 <- HBAN[which(HBAN$Time == ymd_hms("2007-01-03 09:30:00")):which(HBAN$Time == ymd_hms("2007-01-03 16:00:00")),]
data2 <- data2[,1:2]
data1$Time <- lubridate::ymd_hms(paste0("2007-01-03 ",data1$utcsec))
data1 <- data.frame(ymd_hms(data1$Time),data1$price)
data1 <- data1[head(which(data1$ymd_hms.data1.Time. == ymd_hms("2007-01-03 12:00:00")),1):
                 tail(which(data1$ymd_hms.data1.Time. == ymd_hms("2007-01-03 13:00:01")),1),]
data1 <- data1 %>% cbind(rep("raw",length(data1[,1])))
colnames(data1) <- c(colnames(data2),"type")
data2 <- data2[,1:2] %>% cbind(rep("clean",length(data2[,1])))
colnames(data2) <- colnames(data1)
data <- rbind(data1,data2)


ggplot(data,aes(x = Time,y = Price)) + ggtitle("HBAN: Clean vs raw data") +
  geom_line(aes(colour = type),size = 0.1) +
  #geom_line(aes(y =  data2$price , x = data2$utcsec, colour = "Cleaned")) +
  scale_color_manual(values = c("red", "black")) + 
  #scale_linetype_manual(values = c("solid","dashed")) +
  theme(legend.title = element_blank() )+
  xlab("Time")+ ylab("Price")

############################################Intro plots#####################################

y0 <- c(1., 2., 4., 3.)
sfun0  <- stepfun(1:3, y0, f = 0)
sfun.2 <- stepfun(1:3, y0, f = 0.2)
sfun1  <- stepfun(1:3, y0, f = 1)
sfun1c <- stepfun(1:3, y0, right = TRUE) # hence f=1
sfun0
summary(sfun0)
summary(sfun.2)
plot(stepfun(ACAS_5minute$Time,cumsum(c(0,(ACAS_5minute$logr)))))

Browsim = function(B0, T, delta, d, grid){
  B = matrix(0,length(grid),d)
  B[1,] = B0
  for(i in 2:length(grid)){
    e = rnorm(d,0,sqrt(delta))
    B[i,] = B[i-1,] + e
  }
  
  return(B) 
}

plot(Browsim(0,1,1/99382,1,seq(0,99382,1)),type="l")

ACAS2008 <- ACAS[which(ACAS$Time == ymd_hms("2008-01-03 09:30:00")):which(ACAS$Time == ymd_hms("2008-12-31 16:00:00")),]

ACAS_5minute = ACAS[,1:2] %>% filter(minute(Time) %% 5 == 0)

ACAS_5minute %<>% logreturns %<>% simreturns

ACASstep <- stepfun(1:length(ACAS_5minute$Time),cumsum(c(0,(ACAS_5minute$logr))))
ACASstep <- stepfun(1:length(ACAS_5minute$Time),cumsum(c(0,(ACAS_5minute$logr))))
which(ACAS$Time == ymd_hms("2008-01-03 09:30:00"))which(ACAS$Time == ymd_hms("2008-12-31 16:00:00"))

par(mfrow=c(1,1))
set.seed(12334)
plot(ACASstep,xlim=c(which(ACAS_5minute$Time == ymd_hms("2008-01-03 09:30:00")),which(ACAS_5minute$Time == ymd_hms("2008-06-30 16:00:00"))),
     vertical=T,do.points=F,main=" ", 
     ylim=c(-0.5,0.5),
     xaxt='n',xlab=" ",
     yaxt='n',ylab=" "
)
plot(Browsim(0,1,1/99382,1,seq(0,99382,1))[1:10000],type="l",ylim=c(-0.5,0.5),
     xaxt='n',xlab=" ",
     yaxt='n',ylab=" "
)
which(ACAS_5minute$Time == ymd_hms("2008-01-03 09:30:00"));which(ACAS_5minute$Time == ymd_hms("2008-12-31 16:00:00"))

plot(ACAS_5minute$logr[59567:79474],type="l",ylim=c(-0.1,0.1),ylab="Log returns",xaxt='n',xlab=" ")
plot(rnorm(19907,sd=sqrt(1/19907)),type="l",ylim=c(-0.1,0.1),ylab="Increments",xaxt='n',xlab=" ")

######################################weight plot###############################################

wperiode <- periode[seq(1,length(periode),by=79)]

par(mfrow=c(2,1))
plot(wperiode,pweights[[1]][,1],type="l",ylab="weights",xlab="Time",main="SPY")
lines(wperiode,pweights[[3]][,1],col="red")
legend("topleft",legend = c("CVAR","MVP"),col = c("black","red"),lty=1)

plot(wperiode,pweights[[1]][,2],type="l",ylab="weights",xlab="Time",main="ACAS")
lines(wperiode,pweights[[3]][,2],col="red")
legend("topleft",legend = c("CVAR","MVP"),col = c("black","red"),lty=1)

plot(wperiode,pweights[[1]][,3],type="l",ylab="weights",xlab="Time",main="HBAN")
lines(wperiode,pweights[[3]][,3],col="red")
legend("topleft",legend = c("CVAR","MVP"),col = c("black","red"),lty=1)

####################################simple returns#############################################

par(mfrow=c(1,1))
plot(ACAS$Time,ACAS$simr,type="l",xlab="Time",ylab="Simple returns")
plot(HBAN$Time,HBAN$simr,type="l",xlab="Time",ylab="Simple returns")
plot(SPY$Time,SPY$simr,type="l",xlab="Time",ylab="Simple returns")



###################################CVAR cumsum returns#######################################

cumreturns <- cumsum(log(returns$expected.shortfall+1))





