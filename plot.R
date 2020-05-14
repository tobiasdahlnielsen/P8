


load("../P8/Rdata/Clean_Data_5minutes.Rdata")
load("WFC20081014.rdata")
data1 <- HBAN_20070103
data1 <- data1[,c(1,3)]
#collect <- tbl(DB, "WFC") %>%  collect() %>% arrange(utcsec)
data2 <- HBAN[which(HBAN$Time == ymd_hms("2007-01-03 09:30:00")):which(HBAN$Time == ymd_hms("2007-01-03 16:00:00")),]
data2 <- data2[,1:2]
data1$Time <- lubridate::ymd_hms(paste0("2007-01-03 ",data1$utcsec))
data1 <- data.frame(ymd_hms(data1$Time),data1$price)
data1 <- data1 %>% cbind(rep("raw",length(data1[,1])))
colnames(data1) <- c(colnames(data2),"type")
data2 <- data2[,1:2] %>% cbind(rep("clean",length(data2[,1])))
colnames(data2) <- colnames(data1)
data <- rbind(data1,data2)

#dbDisconnect(DB)

ggplot(data,aes(x = Time,y = Price)) + ggtitle("HBAN: Clean vs raw data") +
  geom_line(aes(colour = type),size = 0.1) +
  #geom_line(aes(y =  data2$price , x = data2$utcsec, colour = "Cleaned")) +
  scale_color_manual(values = c("red", "black")) + 
  #scale_linetype_manual(values = c("solid","dashed")) +
  theme(legend.title = element_blank() )+
  xlab("Time")+ ylab("Price")

