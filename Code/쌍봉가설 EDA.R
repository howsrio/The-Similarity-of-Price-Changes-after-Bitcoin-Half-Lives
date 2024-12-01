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
nrow(BTC)
TargetDate <- as.Date(c('2012-11-28','2016-07-09','2020-05-11'))
# 반감기 1 2012-11-28
TargetIndex1 <- which(BTC$Date == TargetDate[1])
dual.BTC1=BTC[(TargetIndex1):(TargetIndex1+(365*1.5)),]
MaxIndex1.1 <- which(dual.BTC1$Close==max(dual.BTC1$Close)) #반감기이후 1차 최고점
MaxIndex1.2 <- which(dual.BTC1$Close==max(dual.BTC1$Close[(MaxIndex1.1+30):nrow(dual.BTC1)])) # 2차 최고점
MinIndex1.1 <- which(dual.BTC1$Close==min(dual.BTC1$Close[MaxIndex1.1:MaxIndex1.2]))
MinIndex1.2 <- which(dual.BTC1$Close == min(dual.BTC1$Close[MaxIndex1.2:nrow(dual.BTC1)]))
MaxIndex1.3 <- which(dual.BTC1$Close == max(dual.BTC1$Close[MinIndex1.2:(MinIndex1.2+100)]))


dual.BTC1$Close[1]
dual.BTC1$Close[MaxIndex1.1] #9980%
dual.BTC1$Close[MinIndex1.1] #43%
dual.BTC1$Close[MaxIndex1.2] #180%
dual.BTC1$Close[MinIndex1.2] #11%
dual.BTC1$Close[MaxIndex1.3] #593%


# 반감기~ 2년
ggplot(dual.BTC1)+
  geom_line(aes(x=Date,y=Close))+
  geom_vline(xintercept = as.numeric(dual.BTC1$Date[MaxIndex1.1]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC1$Date[MaxIndex1.2]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC1$Date[MinIndex1.1]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC1$Date[MinIndex1.2]), color = "red", linetype = "dashed") 


# 반감기 2 
TargetIndex2 <- which(BTC$Date == TargetDate[2])
dual.BTC2=BTC[(TargetIndex2):(TargetIndex2+(365*2)),]
MaxIndex2.1 <- which(dual.BTC2$Close==max(dual.BTC2$Close)) #반감기이후 2차 최고점
MaxIndex2.2 <- which(dual.BTC2$Close==max(dual.BTC2$Close[(MaxIndex2.1+10):nrow(dual.BTC2)])) # 2차 최고점
MinIndex2.1 <- which(dual.BTC2$Close==min(dual.BTC2$Close[MaxIndex2.1:MaxIndex2.2]))
MinIndex2.2 <- which(dual.BTC2$Close == min(dual.BTC2$Close[MaxIndex2.2:(MaxIndex2.2+30)]))
MaxIndex2.3 <- which(dual.BTC2$Close == max(dual.BTC2$Close[MinIndex2.2:(MinIndex2.2+20)]))





dual.BTC2$Close[1] 
dual.BTC2$Close[MaxIndex2.1] #2968%
dual.BTC2$Close[MinIndex2.1] #64%
dual.BTC2$Close[MaxIndex2.2] #137%
dual.BTC2$Close[MinIndex2.2] #40%
dual.BTC2$Close[MaxIndex2.3] #162%


# 반감기~ 2년
ggplot(dual.BTC2)+
  geom_line(aes(x=Date,y=Close))+
  geom_vline(xintercept = as.numeric(dual.BTC2$Date[MaxIndex2.1]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC2$Date[MaxIndex2.2]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC2$Date[MinIndex2.1]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC2$Date[MinIndex2.2]), color = "red", linetype = "dashed") # 반감기~ 2년
ggplot(dual.BTC2[(MaxIndex2.1-50):(MaxIndex2.3+50),])+
  geom_line(aes(x=Date,y=Close))+
  geom_vline(xintercept = as.numeric(dual.BTC2$Date[MaxIndex2.1]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC2$Date[MaxIndex2.2]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC2$Date[MinIndex2.1]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC2$Date[MinIndex2.2]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC2$Date[MaxIndex2.3]), color = "red", linetype = "dashed") 


# 반감기 3 
TargetIndex3 <- which(BTC$Date == TargetDate[3])
dual.BTC3=BTC[(TargetIndex3):(TargetIndex3+(365*2.5)),]
MaxIndex3.1 <- which(dual.BTC3$Close==max(dual.BTC3$Close[1:365])) 
MaxIndex3.2 <- which(dual.BTC3$Close==max(dual.BTC3$Close[(MaxIndex3.1+10):nrow(dual.BTC3)])) # 3차 최고점
MinIndex3.1 <- which(dual.BTC3$Close==min(dual.BTC3$Close[MaxIndex3.1:MaxIndex3.2]))
MinIndex3.2 <- which(dual.BTC3$Close == min(dual.BTC3$Close[MaxIndex3.2:(nrow(dual.BTC3)-120)]))
MaxIndex3.3 <- which(dual.BTC3$Close == max(dual.BTC3$Close[MinIndex3.2:(MinIndex3.2+80)]))

dual.BTC3$Close[1]
dual.BTC3$Close[MaxIndex3.1] #740%
dual.BTC3$Close[MinIndex3.1] #47%
dual.BTC3$Close[MaxIndex3.2] #227%
dual.BTC3$Close[MinIndex3.2] #28%
dual.BTC3$Close[MaxIndex3.3] #128%


# 반감기~ 2년
ggplot(dual.BTC3)+
  geom_line(aes(x=Date,y=Close))+
  geom_vline(xintercept = as.numeric(dual.BTC3$Date[MaxIndex3.1]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC3$Date[MaxIndex3.2]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC3$Date[MinIndex3.1]), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(dual.BTC3$Date[MinIndex3.2]), color = "red", linetype = "dashed") 

