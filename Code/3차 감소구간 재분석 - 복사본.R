#3-1차 감소 분석
#3-1차 감소률: 1/2.1배
max(df3down2$Close)/min(df3down2$Close)
#3차 기간:98
period3.down <- df3down$Date[nrow(df3down2)]-df3down2$Date[1]
df3down <- df3down2
df3down.scaled=cbind(1:nrow(df3down)/nrow(df3down),df3down$Volume,df3down$Close-min(df3down$Close))
df3down.scaled[,2]=df3down.scaled[,2]/max(df3down.scaled[,2],na.rm = TRUE)
df3down.scaled[,3]=df3down.scaled[,3]/max(df3down.scaled[,3])
colnames(df3down.scaled)=c('Time','Volume','Close')
df3down.scaled <- as.data.frame(df3down.scaled)
head(df3down.scaled)

#lm(Close ~ Time, df1down.scaled)
segmented_model3 <- segmented(lm(Close ~ Time, df3down.scaled), seg.Z = ~ Time, 
                              psi = c(quantile(df3down.scaled$Time, c(0.25,0.5,0.75))))
#summary(segmented_model)
visualize_segmented_model(df3down.scaled$Time,df3down.scaled$Close,segmented_model3)

term <- 15
seg.term = round(period3.down/term,0)
seg <- seq(0,1,length.out = (seg.term+2))[2:seg.term]
df3down.scaled2$Time <- df3down.scaled2$Time/max(df3down.scaled2$Time)
segmented_model3.2 <- segmented(lm(Close ~ Time, df3down.scaled2), seg.Z = ~ Time, 
                                psi = c(quantile(df3down.scaled2$Time, seg)))
visualize_segmented_model(df3down.scaled2$Time,df3down.scaled2$Close,segmented_model3.2)

pred <- predict(segmented_model3)
breakpoints <- round(segmented_model3.2$psi[,'Est.']*period3.down,0)
breakpoints <- c(1,breakpoints,period3.down)
break.down3 <- pred[breakpoints]

