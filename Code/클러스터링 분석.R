{
library(readxl)
library(ggplot2)
library(dtw)
library(readr)


BTC_data <- read_csv("BTC  data.csv")
colnames(BTC_data)=c('Date', 'Close', 'Open', 'High', 'Low' ,'Volume' ,'Change')
BTC_data$Date=as.Date(BTC_data$Date)
BTC_data$Change <-  as.numeric(sub("%", "", BTC_data$Change))
BTC_data$Volume <-  as.numeric(sub("K", "", BTC_data$Volume))

BTC  <- BTC_data[nrow(BTC_data):1,]
TargetDate <- as.Date(c('2012-11-28','2016-07-09','2020-05-11'))

TargetIndex1 <- which(BTC$Date == TargetDate[1])
dual.BTC1=BTC[(TargetIndex1):(TargetIndex1+(365*2)),]
MaxIndex1.1 <- which(dual.BTC1$Close==max(dual.BTC1$Close))
MaxIndex1.2 <- which(dual.BTC1$Close==max(dual.BTC1$Close[(MaxIndex1.1+30):nrow(dual.BTC1)])) # 2차 최고점
MinIndex1.1 <- which(dual.BTC1$Close==min(dual.BTC1$Close[MaxIndex1.1:MaxIndex1.2]))
MinIndex1.2 <- which(dual.BTC1$Close == min(dual.BTC1$Close[MaxIndex1.2:nrow(dual.BTC1)]))
MaxIndex1.3 <- which(dual.BTC1$Close == max(dual.BTC1$Close[MinIndex1.2:(MinIndex1.2+100)]))

TargetIndex2 <- which(BTC$Date == TargetDate[2])
dual.BTC2=BTC[(TargetIndex2):(TargetIndex2+(365*2)),]
MaxIndex2.1 <- which(dual.BTC2$Close==max(dual.BTC2$Close))
MaxIndex2.2 <- which(dual.BTC2$Close==max(dual.BTC2$Close[(MaxIndex2.1+10):nrow(dual.BTC2)])) # 2차 최고점
MinIndex2.1 <- which(dual.BTC2$Close==min(dual.BTC2$Close[MaxIndex2.1:MaxIndex2.2]))
MinIndex2.2 <- which(dual.BTC2$Close == min(dual.BTC2$Close[MaxIndex2.2:(MaxIndex2.2+30)]))
MaxIndex2.3 <- which(dual.BTC2$Close == max(dual.BTC2$Close[MinIndex2.2:(MinIndex2.2+20)]))

TargetIndex3 <- which(BTC$Date == TargetDate[3])
dual.BTC3=BTC[(TargetIndex3):(TargetIndex3+(365*2.5)),]
MaxIndex3.1 <- which(dual.BTC3$Close==max(dual.BTC3$Close[1:365])) 
MaxIndex3.2 <- which(dual.BTC3$Close==max(dual.BTC3$Close[(MaxIndex3.1+10):nrow(dual.BTC3)])) # 3차 최고점
MinIndex3.1 <- which(dual.BTC3$Close==min(dual.BTC3$Close[MaxIndex3.1:MaxIndex3.2]))
MinIndex3.2 <- which(dual.BTC3$Close == min(dual.BTC3$Close[MaxIndex3.2:(nrow(dual.BTC3)-120)]))
MaxIndex3.3 <- which(dual.BTC3$Close == max(dual.BTC3$Close[MinIndex3.2:(MinIndex3.2+80)]))

compare.BTC1.df <-  read_excel("BTC1 test dataset.xlsx")
compare.BTC2.df <-  read_excel("BTC2 test dataset.xlsx")
compare.BTC3.df <-  read_excel("BTC3 test dataset.xlsx")
}

head(compare.BTC1.df)

#겹치는 부분이 있는 곳의 거리 확인
hist(compare.BTC1.df$rate.list,main=)
hist(compare.BTC1.df$include.list)

plot(compare.BTC1.df$size.list,1/compare.BTC1.df$norm.dist.list,type='h')
plot(compare.BTC1.df$start.list,1/compare.BTC1.df$norm.dist.list,type='h')

BTC1.none0include <- compare.BTC1.df[compare.BTC1.df$include.list != 0,]

plot(compare.BTC1.df$include.list,compare.BTC1.df$norm.dist.list)
# include값이 0이외의 값에선 다양한 값이 관측되지 않는다.
plot(BTC1.none0include$include.list,BTC1.none0include$norm.dist.list)
#우리의 목표 값의 최대치가 0.34인것을 고려해 0.1까지 데이터는 가저갈 만해 보임임

BTC1.0rate <- compare.BTC1.df[compare.BTC1.df$rate.list == 0,]
boxplot(BTC1.0rate$norm.dist.list)

(BTC1.0rate[order(BTC1.0rate$norm.dist.list),])
BTC1.0rate[order(BTC1.0rate$norm.dist.list),][10000,]

hist(BTC1.0rate$norm.dist.list)
hist(compare.BTC1.df$norm.dist.list)



plot(compare.BTC1.df$size.list,1/compare.BTC1.df$norm.dist.list,type='h',xlab='Size',ylab='Time Smilarity')
plot(compare.BTC1.df$start.list,1/compare.BTC1.df$norm.dist.list,type='h',xlab='Start Index',ylab='Time Smilarity')
range(compare.BTC1.df$start.list[1/compare.BTC1.df$norm.dist.list>8*mean(1/compare.BTC1.df$norm.dist.list)])


plot(BTC1.0rate$size.list,1/BTC1.0rate$norm.dist.list,type='h',xlab='Size',ylab='Time Smilarity')
plot(BTC1.0rate$start.list,1/BTC1.0rate$norm.dist.list,type='h',xlab='Start Index',ylab='Time Smilarity')
BTC1.0rate$size.list


which(BTC1.0rate$norm.dist.list == min(BTC1.0rate$norm.dist.list))
#최고점 인댁스 22702
BTC1.0rate[69423,] #size:898, start:1862
#size:1128  start:1632
BTC1.1800 <- BTC1.0rate[(BTC1.0rate$start.list<1922) & (BTC1.0rate$start.list>1802),]
plot(BTC1.1800$size.list,1/BTC1.1800$norm.dist.list, type='h',,xlab='Size',ylab='Time Similarity')
A <- which(BTC1.1800$norm.dist.list == min(BTC1.1800$norm.dist.list))
BTC1.1800[A,]

hist(BTC1.1800$norm.dist.list)

min.ndist1.1 <- BTC[1862:(1862+946),]
plot(min.ndist1.1$Close,type='l',ylab='Close')
plot(dual.BTC2$Close[1:MinIndex2.2],type='l',ylab='Close')


#유사도가 가장 높은 구간
ggplot(BTC[1814:(1814+946),])+
  geom_line(aes(x=Date,y=Close))



#2차 반감기 이후 구간간
ggplot(BTC[TargetIndex2:(TargetIndex2+MinIndex2.2),])+
  geom_line(aes(x=Date,y=Close))



BTC1.scaled <- dual.BTC1$Close[1:MinIndex1.2]
BTC1.scaled <- BTC1.scaled/(max(BTC1.scaled)-min(BTC1.scaled))
min.ndist1.1.scaled <- min.ndist1.1$Close/(max(min.ndist1.1$Close)-min(min.ndist1.1$Close))

dtw.best1 <- dtw(BTC1.scaled,min.ndist1.1.scaled)
plot(dtw.best1)


df <- BTC1.0rate[BTC1.0rate$start.list<1000,]
which(df$norm.dist.list==min(df$norm.dist.list))
#2번째 고점 index: df의 15637
df[1326,] #size:371, start:16

df <- BTC1.0rate[BTC1.0rate$start.list>3000,]
which(df$norm.dist.list==min(df$norm.dist.list))
#2번째 고점 index: df의 15637
df[15637,] #size:900, start:3119

BTC1.3100 <- BTC1.0rate[(BTC1.0rate$start.list>3059) & (BTC1.0rate$start.list<3179),]
plot(BTC1.3100$size.list,1/BTC1.3100$norm.dist.list, type='h')
hist(BTC1.3100$norm.dist.list)]
max(BTC1.3100$norm.dist.list)


min.ndist1.2 <- BTC[3119:(900+3119),]


#유사도가 가장 높은 구간
ggplot(BTC[3119:(3119+898),])+
  geom_line(aes(x=Date,y=Close))

#2차 반감기 이후 구간간
ggplot(BTC[TargetIndex3:(TargetIndex3+MinIndex3.2),])+
  geom_line(aes(x=Date,y=Close))


plot(min.ndist1.2$Close,type='l')
plot(dual.BTC3$Close[1:MinIndex3.2],type='l')










#------- BTC2
compare.BTC2.df
plot(compare.BTC2.df$start.list,1/compare.BTC2.df$norm.dist.list,type='h')

BTC2.0rate <- compare.BTC2.df[compare.BTC2.df$rate.list <0.1,]

plot(BTC2.0rate$size.list,1/BTC2.0rate$norm.dist.list,type='h')
plot(BTC2.0rate$start.list,1/BTC2.0rate$norm.dist.list,type='h')


#------- BTC3
plot(compare.BTC3.df$start.list,1/compare.BTC3.df$norm.dist.list,type='h')

BTC3.0rate <- compare.BTC3.df[compare.BTC3.df$rate.list <0.1,]

plot(BTC3.0rate$size.list,1/BTC3.0rate$norm.dist.list,type='h')
plot(BTC3.0rate$start.list,1/BTC3.0rate$norm.dist.list,type='h')
TargetIndex3


#-----BTC3 분석

head(compare.BTC3.df)

#겹치는 부분이 있는 곳의 거리 확인
hist(compare.BTC3.df$rate.list,main=)
hist(compare.BTC3.df$include.list)

plot(compare.BTC3.df$size.list,1/compare.BTC3.df$norm.dist.list,type='h')
plot(compare.BTC3.df$start.list,1/compare.BTC3.df$norm.dist.list,type='h')

BTC3.none0include <- compare.BTC3.df[compare.BTC3.df$include.list != 0,]

plot(compare.BTC3.df$include.list,compare.BTC3.df$norm.dist.list)
# include값이 0이외의 값에선 다양한 값이 관측되지 않는다.
plot(BTC3.none0include$include.list,BTC3.none0include$norm.dist.list)
#우리의 목표 값의 최대치가 0.34인것을 고려해 0.1까지 데이터는 가저갈 만해 보임임

BTC3.0rate <- compare.BTC3.df[compare.BTC3.df$rate.list == 0,]
boxplot(BTC3.0rate$norm.dist.list)

(BTC3.0rate[order(BTC3.0rate$norm.dist.list),])
BTC3.0rate[order(BTC1.0rate$norm.dist.list),][10000,]


plot(BTC3.0rate$size.list,1/BTC3.0rate$norm.dist.list,type='h',xlab='Size',ylab='Time Smilarity')
#start에 때른 Time
plot(BTC3.0rate$start.list,1/BTC3.0rate$norm.dist.list,type='h',xlab='Start Index',ylab='Time Similarity')


which(BTC3.0rate$norm.dist.list == min(BTC3.0rate$norm.dist.list))
#최고점 인댁스 12077
BTC3.0rate[12077,] #size:550, start:2495

BTC3.1800 <- BTC3.0rate[(BTC3.0rate$start.list<2555) & (BTC3.0rate$start.list>2435),]

#size에 따른 Time
plot(BTC3.1800$size.list,1/BTC3.1800$norm.dist.list, type='h',,xlab='Size',ylab='Time Similarity')

A <- which(BTC3.1800$norm.dist.list == min(BTC3.1800$norm.dist.list))
BTC3.1800[A,]


min.ndist3.1 <- BTC[1862:(1862+946),]
plot(min.ndist1.1$Close,type='l',ylab='Close')
plot(dual.BTC2$Close[1:MinIndex2.2],type='l',ylab='Close')

summary(BTC[2495:(2495+550),])

#유사도가 가장 높은 구간
ggplot(BTC[2495:(2495+550),])+
  geom_line(aes(x=Date,y=Close))



#2차 반감기 이후 구간간
ggplot(BTC[TargetIndex2:(TargetIndex2+MinIndex2.2),])+
  geom_line(aes(x=Date,y=Close))




