library('segmented')
library('dtw')

BTC1.df <- dual.BTC1[1:MaxIndex1.3,]
BTC2.df <- dual.BTC2[1:MaxIndex2.3,]
BTC3.df <- dual.BTC3[1:MaxIndex3.3,]
Time <- 1:nrow(BTC1.df)
BTC1.df <- cbind(Time,BTC1.df)
Time <- 1:nrow(BTC2.df)
BTC2.df <- cbind(Time,BTC2.df)
Time <- 1:nrow(BTC3.df)
BTC3.df <- cbind(Time,BTC3.df)

plot(BTC1.df$Close)
plot(BTC2.df$Close)
plot(BTC3.df$Close)




BTC1.scaled <- BTC1.df$Close / (max(BTC1.df$Close)-min(BTC1.df$Close))
BTC2.scaled <- BTC2.df$Close / (max(BTC2.df$Close)-min(BTC2.df$Close))
BTC3.scaled <- BTC3.df$Close / (max(BTC3.df$Close)-min(BTC3.df$Close))


dtw12 <- dtw(BTC1.scaled,BTC2.scaled,keep=TRUE,step=asymmetric)
plot(dtw12)
cat(dtw12$distance,dtw12$normalizedDistance)

dtw13 <- dtw(BTC1.scaled,BTC3.scaled,keep=TRUE,step=asymmetric)
plot(dtw13)
cat(dtw13$distance,dtw13$normalizedDistance)

dtw21 <- dtw(BTC2.scaled,BTC1.scaled,keep=TRUE,step=asymmetric)
plot(dtw21)
cat(dtw21$distance,dtw21$normalizedDistance)


dtw23 <- dtw(BTC2.scaled,BTC3.scaled,keep=TRUE,step=asymmetric)
plot(dtw23)
cat(dtw23$distance,dtw23$normalizedDistance)

dtw31 <- dtw(BTC3.scaled,BTC1.scaled,keep=TRUE,step=asymmetric)
plot(dtw31)
cat(dtw31$distance,dtw31$normalizedDistance)

dtw32 <- dtw(BTC3.scaled,BTC2.scaled,keep=TRUE,step=asymmetric)
plot(dtw32)
cat(dtw32$distance,dtw32$normalizedDistance)

dtw31$distance
dtw32$distance
dtw13$distance
dtw12$distance



plot(BTC1.df$Time, BTC1.df$Close, xlab = "Time", ylab = "Price")
segmented_model1.1 <- segmented(lm(Close ~ Time, BTC1.df[1:MaxIndex1.1,]), seg.Z = ~ Time, 
                              psi = c(quantile(BTC1.df$Time, c(0.5))))
predicted_values1.1 <- predict(segmented_model1.1)
lines(BTC1.df$Time[1:MaxIndex1.1], predicted_values1.1, col = "red")
linear_model1.2 <- lm(Close~Time, BTC1.df[MaxIndex1.1:MinIndex1.1,])
predicted_values1.2 <- predict(linear_model1.2)
lines(BTC1.df$Time[MaxIndex1.1:MinIndex1.1], predicted_values1.2, col = "red")

legend("topleft", legend = c("Original", "Fitted"), col = c("black", "red"), lty = 1)
