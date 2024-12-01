library(readr)
library(ggplot2)
#install.packages('magrittr')

BTC_data <- read_csv("BTC  data.csv")

colnames(BTC_data)=c('Date', 'Close', 'Open', 'High', 'Low' ,'Volume' ,'Change')
BTC_data$Date=as.Date(BTC_data$Date)

head(BTC_data)
tail(BTC_data)

BTC_data$Change <-  as.numeric(sub("%", "", BTC_data$Change))

BTC_data$Volume <-  as.numeric(sub("K", "", BTC_data$Volume))

plot(BTC_data$Close)

#순서 역순으로 정렬
BTC  <- BTC_data[nrow(BTC_data):1,]

TargetDate <- as.Date(c('2012-11-28','2016-07-09','2020-05-11'))
# 반감기 1 2012-11-28
TargetIndex <- which(BTC$Date == TargetDate[1])
BTC1=BTC[(TargetIndex-365):(TargetIndex+(365*3),]
tail(BTC1)
#최고점 확인
MaxIndex1 <- which(BTC1$Close==max(BTC1$Close)) - 365
MinIndex1_2 <- which(BTC1$Close==min(BTC1$Close[(MaxIndex1+365):nrow(BTC1)])) - 365
MinIndex1 <- which(BTC1$Close==min(BTC1$Close[(MaxIndex1+365*2):nrow(BTC1)])) - 365
EndIndex1 <- MinIndex1 + 240



ggplot(BTC1)+
  geom_line(aes(x=Date,y=Close))+
  geom_vline(xintercept = as.numeric(TargetDate[1]), color = "red", linetype = "dashed")+
  annotate("text", x = TargetDate[1]-80, y = max(BTC1$Close)-150, label = "D-day", vjust = -1, color = "black") +
  annotate("text", x = TargetDate[1]+MaxIndex1-120, y = max(BTC1$Close)-150, label = 
             ("2013/12/05"), vjust = -1, color = "black") +
  geom_vline(xintercept = as.numeric(TargetDate[1]+MaxIndex1), color = "blue", linetype = "dashed") +
  annotate("text", x = TargetDate[1]+MinIndex1+120, y = max(BTC1$Close)-150, label = "2015/01/15", vjust = -1, color = "black") +
  geom_vline(xintercept = as.numeric(TargetDate[1]+MinIndex1), color = "blue", linetype = "dashed") +
  annotate("text", x = TargetDate[1]+MinIndex1_2+120, y = max(BTC1$Close)-150, label = "2014/02/22", vjust = -1, color = "black") +
  geom_vline(xintercept = as.numeric(TargetDate[1]+MinIndex1_2), color = "blue", linetype = "dashed")

#반감기 2 2016-7-9
#반감기 2 2016-7-9
TargetIndex <- which(BTC$Date == TargetDate[2])
BTC2=BTC[(TargetIndex-365):(TargetIndex+(365*3)),]
MaxIndex2 <- which(BTC2$Close==max(BTC2$Close)) - 365
MinIndex2 <- which(BTC2$Close==min(BTC2$Close[(MaxIndex2+365):nrow(BTC2)])) - 365
EndIndex2 <- MinIndex2 + 90

ggplot(BTC2)+
  geom_line(aes(x=Date,y=Close))+
  geom_vline(xintercept = as.numeric(TargetDate[2]), color = "red", linetype = "dashed")+
  annotate("text", x = TargetDate[2]-80, y = max(BTC2$Close)-2000, label = "D-day", vjust = -1, color = "black") +
  geom_vline(xintercept = as.numeric(TargetDate[2]+MaxIndex2), color = "blue", linetype = "dashed")+
  annotate("text", x = TargetDate[2]+MaxIndex2-120, y = max(BTC2$Close)-2000, label = "2017/12/17", vjust = -1, color = "black") +
  geom_vline(xintercept = as.numeric(TargetDate[2]+MinIndex2), color = "blue", linetype = "dashed")+
  annotate("text", x = TargetDate[2]+MinIndex2-120, y = max(BTC2$Close)-2000, label = "2018/12/16", vjust = -1, color = "black") 

# 반감기 2020-5-11
TargetIndex <- which(BTC$Date == TargetDate[3])
BTC3=BTC[(TargetIndex-365):(TargetIndex+(365*3)),]
MaxIndex3_1 <- which(BTC3$Close==max(BTC3$Close)) - 365
MaxIndex3_2 <- which(BTC3$Close==max(BTC3$Close[1:(MaxIndex3_1-100+365)])) - 365
MinIndex3_2 <- which(BTC3$Close==min(BTC3$Close[(MaxIndex3_2+365):(MaxIndex3_1+365)])) - 365
MinIndex3_1 <- which(BTC3$Close==min(BTC3$Close[(MaxIndex3_1+365):nrow(BTC3)])) - 365
ggplot(BTC3)+
  geom_line(aes(x=Date,y=Close))+
  geom_vline(xintercept = as.numeric(TargetDate[3]), color = "red", linetype = "dashed")+
  annotate("text", x = TargetDate[3]-60, y = max(BTC3$Close)-7000, label = "D-day", vjust = -1, color = "black") +
  #geom_vline(xintercept = as.numeric(TargetDate[3]+365), color = "red", linetype = "dashed")+
  #geom_vline(xintercept = as.numeric(TargetDate[3]+365), color = "red", linetype = "dashed")+
  geom_vline(xintercept = as.numeric(TargetDate[3]+MaxIndex3_1), color = "blue", linetype = "dashed")+
  geom_vline(xintercept = as.numeric(TargetDate[3]+MaxIndex3_1), color = "blue", linetype = "dashed")+
  annotate("text", x = TargetDate[3]+MaxIndex3_1+100, y = max(BTC3$Close)-8000, label = "2021/11/09", vjust = -1, color = "black") +
  geom_vline(xintercept = as.numeric(TargetDate[3]+MaxIndex3_2), color = "blue", linetype = "dashed")+
  annotate("text", x = TargetDate[3]+MaxIndex3_2-100, y = max(BTC3$Close)-7000, label = "2021/04/14", vjust = -1, color = "black") +
  geom_vline(xintercept = as.numeric(TargetDate[3]+MinIndex3_2), color = "blue", linetype = "dashed")+
  annotate("text", x = TargetDate[3]+MinIndex3_2+100, y = max(BTC3$Close)-5000, label = "2021-07-21", vjust = -1, color = "black")+
  geom_vline(xintercept = as.numeric(TargetDate[3]+MinIndex3_1), color = "blue", linetype = "dashed")+
  annotate("text", x = TargetDate[3]+MinIndex3_1+100, y = max(BTC3$Close)-7000, label = "2022/11/22", vjust = -1, color = "black")





