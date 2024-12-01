library(tseries)
library(forecast)
library(ggplot2)

#BTC1 전처리
df1up=BTC1[365:(MaxIndex1+365),] #최고점~최저점 
df1down=BTC1[(MaxIndex1+365):(MinIndex1+365),] #반간기~ 최고점

#1차 반감기 상승구간
ggplot(df1up)+
  geom_line(aes(x=Date,y=Close))
#1차 반감기 감소구간
ggplot(df1down)+
  geom_line(aes(x=Date,y=Close))
#--------------
#BTC2 전처리
df2up=BTC2[365:(MaxIndex2+365),] #최고점~최저점 
df2down=BTC2[(MaxIndex2+365):(MinIndex2+365),] #반간기~ 최고점

#2차 반감기 상승구간
ggplot(df2up)+
  geom_line(aes(x=Date,y=Close))
#2차 반감기 감소구간
ggplot(df2down)+
  geom_line(aes(x=Date,y=Close))
#----------------
#BTC3 전처리
df3up=BTC3[365:(MaxIndex3_2+365),] #최고점~최저점 
df3down1=BTC3[(MaxIndex3_1+365):(MinIndex3_1+365),] #반간기~ 최고점
df3down2=BTC3[(MaxIndex3_2+365):(MinIndex3_2+365),] #반간기~ 최고점

#3차 반감기 상승구간
ggplot(df3up)+
  geom_line(aes(x=Date,y=Close))
#3차 반감기 감소구간
ggplot(df3down1)+
  geom_line(aes(x=Date,y=Close))

ggplot(df3down2)+
  geom_line(aes(x=Date,y=Close))


