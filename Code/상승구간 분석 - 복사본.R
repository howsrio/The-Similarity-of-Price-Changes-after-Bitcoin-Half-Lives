library(ggplot2)
library(tseries)
library(forecast)
library(segmented)

#1차 상승률: 100배
max(df1up$Close)/min(df1up$Close)
#1차 기간:372
period1.up <- df1up$Date[nrow(df1up)]-df1up$Date[1]

#2차 상승률: 37배
max(df2up$Close)/min(df2up$Close)
#2차 기간:526
period2.up <- df2up$Date[nrow(df2up)]-df2up$Date[1]

#2차 상승률: 7.4배
max(df3up$Close)/min(df3up$Close)
#2차 기간:338
period3.up <- df3up$Date[nrow(df3up)]-df3up$Date[1]


#acf,pacf 확인
par(mfrow=c(3, 2))
pacf(df1up$Close)
acf(df1up$Close) 
pacf(df2up$Close)
acf(df2up$Close) 
pacf(df3up$Close)
acf(df3up$Close) 

diff=2
df1up.dif= diff(df1up$Close, differences = diff)
adf_result <- adf.test(df1up.dif)
print(adf_result)
df2up.dif= diff(df2up$Close, differences = diff)
adf_result <- adf.test(df2up.dif)
print(adf_result)
df3up.dif= diff(df3up$Close, differences = diff)
adf_result <- adf.test(df3up.dif)
print(adf_result)

par(mfrow=c(3, 2))
pacf(df1up.dif)
acf(df1up.dif) 
pacf(df2up.dif)
acf(df2up.dif) 
pacf(df3up.dif)
acf(df3up.dif) 


#모델링
auto.arima(df1up$Close,max.d = 1)
auto.arima(df2up$Close,max.d = 1)
auto.arima(df3up$Close,max.d = 1)

auto.arima(df1up.dif)
auto.arima(df2up.dif)
auto.arima(df3up.dif)
arima(df1up.dif,order = c(3,0,3))
arima(df2up.dif,order = c(3,0,3))
arima(df3up.dif,order = c(3,0,3))
#---------

visualize_segmented_model <- function(x, y, segmented_model) {
  # 원래 데이터와 예측값 얻기
  predicted_values <- predict(segmented_model)
  
  # 시각화
  plot(x, y, xlab = "Time", ylab = "Price")
  lines(x, predicted_values, col = "red")
  legend("topleft", legend = c("Original", "Fitted"), col = c("black", "red"), lty = 1)
}


df1up.scaled=cbind(1:nrow(df1up)/nrow(df1up),df1up$Volume,df1up$Close-min(df1up$Close))
df1up.scaled[,2]=df1up.scaled[,2]/max(df1up.scaled[,2])
df1up.scaled[,3]=df1up.scaled[,3]/max(df1up.scaled[,3])
colnames(df1up.scaled)=c('Time','Volume','Close')
df1up.scaled <- as.data.frame(df1up.scaled)
head(df1up.scaled)

#lm(Close ~ Time, df1up.scaled)
segmented_model1 <- segmented(lm(Close ~ Time, df1up.scaled), seg.Z = ~ Time,
                              psi = c(quantile(df1up.scaled$Time, c( 0.5))))
#summary(segmented_model)
visualize_segmented_model(df1up.scaled$Time,df1up.scaled$Close,segmented_model1)


df2up.scaled=cbind(1:nrow(df2up)/nrow(df2up),df2up$Volume,df2up$Close-min(df2up$Close))
df2up.scaled[,2]=df2up.scaled[,2]/max(df2up.scaled[,2])
df2up.scaled[,3]=df2up.scaled[,3]/max(df2up.scaled[,3])
colnames(df2up.scaled)=c('Time','Volume','Close')
df2up.scaled <- as.data.frame(df2up.scaled)
head(df2up.scaled)

#lm(Close ~ Time, df1up.scaled)
segmented_model2 <- segmented(lm(Close ~ Time, df2up.scaled), seg.Z = ~ Time, 
                              psi = c(quantile(df2up.scaled$Time, c(0.5))))
#summary(segmented_model)
visualize_segmented_model(df2up.scaled$Time,df2up.scaled$Close,segmented_model2)


df3up.scaled=cbind(1:nrow(df3up)/nrow(df3up),df3up$Volume,df3up$Close-min(df3up$Close))
df3up.scaled[,2]=df3up.scaled[,2]/max(df3up.scaled[,2],na.rm = TRUE)
df3up.scaled[,3]=df3up.scaled[,3]/max(df3up.scaled[,3])
colnames(df3up.scaled)=c('Time','Volume','Close')
df3up.scaled <- as.data.frame(df3up.scaled)
head(df3up.scaled)

#lm(Close ~ Time, df1up.scaled)
segmented_model3 <- segmented(lm(Close ~ Time, df3up.scaled), seg.Z = ~ Time, 
                              psi = c(quantile(df3up.scaled$Time, c(0.5))))
#summary(segmented_model)
visualize_segmented_model(df3up.scaled$Time,df3up.scaled$Close,segmented_model3)

segmented_model3$model$psi1.Time
predict(segmented_model3)



#----
# segmented 분석

term <- 20
seg.term = round(period1.up/term)
seg <- seq(0,1,length.out = (seg.term+2))[2:seg.term]

segmented_model1 <- segmented(lm(Close ~ Time, df1up.scaled), seg.Z = ~ Time,
                              psi = c(quantile(df1up.scaled$Time, seg)))
visualize_segmented_model(df1up.scaled$Time,df1up.scaled$Close,segmented_model1)

pred <- predict(segmented_model1)
breakpoints <- round(segmented_model1$psi[,'Est.']*period1.up,0)
breakpoints <- c(1,breakpoints,period1.up)
break.up1 <- pred[breakpoints]


seg.term = round(period2.up/term,0)
seg <- seq(0,1,length.out = (seg.term+2))[2:seg.term]
segmented_model2 <- segmented(lm(Close ~ Time, df2up.scaled), seg.Z = ~ Time, 
                              psi = c(quantile(df2up.scaled$Time, seg)))
visualize_segmented_model(df2up.scaled$Time,df2up.scaled$Close,segmented_model2)

pred <- predict(segmented_model2)
breakpoints <- round(segmented_model2$psi[,'Est.']*period2.up,0)
breakpoints <- c(1,breakpoints,period2.up)
break.up2 <- pred[breakpoints]


seg.term = round(period3.up/term,0)-1
seg <- seq(0,1,length.out = (seg.term+2))[2:seg.term]
segmented_model3 <- segmented(lm(Close ~ Time, df3up.scaled), seg.Z = ~ Time, 
                                psi = c(quantile(df3up.scaled$Time, seg)))
visualize_segmented_model(df3up.scaled$Time,df3up.scaled$Close,segmented_model3)

pred <- predict(segmented_model3)
breakpoints <- round(segmented_model3$psi[,'Est.']*period3.up,0)
breakpoints <- c(1,breakpoints,period3.up)
break.up3 <- pred[breakpoints]
