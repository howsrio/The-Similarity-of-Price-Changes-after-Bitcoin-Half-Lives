library(segmented)

#1차 감소소률: 1/11배
max(df1down$Close)/min(df1down$Close)
#1차 감소소 기간:79
period1.down=df1down$Date[nrow(df1down)]-df1down$Date[1]

#2차 감소소률: 1/5.9배
max(df2down$Close)/min(df2down$Close)
#2차 기간:364
period2.down <- df2down$Date[nrow(df2down)]-df2down$Date[1]

#3차 감소률: 1/4.2배
max(df3down1$Close)/min(df3down1$Close)
#3차 기간:378
period3.down <- df3down$Date[nrow(df3down1)]-df3down1$Date[1]
df3down <- df3down1
#3차 기간 227
period3.down <- 227

#3-1차 감소률: 1/4.2배
max(df3down2$Close)/min(df3down2$Close)
#3차 기간:378
period3.down <- df3down$Date[nrow(df3down1)]-df3down1$Date[1]
df3down <- df3down1


df1down.scaled=cbind(1:nrow(df1down)/nrow(df1down),df1down$Volume,df1down$Close-min(df1down$Close))
df1down.scaled[,2]=df1down.scaled[,2]/max(df1down.scaled[,2])
df1down.scaled[,3]=df1down.scaled[,3]/max(df1down.scaled[,3])
colnames(df1down.scaled)=c('Time','Volume','Close')
df1down.scaled <- as.data.frame(df1down.scaled)
head(df1down.scaled)

#lm(Close ~ Time, df1down.scaled)
segmented_model1 <- segmented(lm(Close ~ Time, df1down.scaled), seg.Z = ~ Time,
                              psi = c(quantile(df1down.scaled$Time, c( 0.25,0.75))))
#summary(segmented_model)
visualize_segmented_model(df1down.scaled$Time,df1down.scaled$Close,segmented_model1)



df2down.scaled=cbind(1:nrow(df2down)/nrow(df2down),df2down$Volume,df2down$Close-min(df2down$Close))
df2down.scaled[,2]=df2down.scaled[,2]/max(df2down.scaled[,2])
df2down.scaled[,3]=df2down.scaled[,3]/max(df2down.scaled[,3])
colnames(df2down.scaled)=c('Time','Volume','Close')
df2down.scaled <- as.data.frame(df2down.scaled)
head(df2down.scaled)

#lm(Close ~ Time, df1down.scaled)
segmented_model2 <- segmented(lm(Close ~ Time, df2down.scaled), seg.Z = ~ Time, 
                              psi = c(quantile(df2down.scaled$Time, c(0.25,0.75))))
#summary(segmented_model)
visualize_segmented_model(df2down.scaled$Time,df2down.scaled$Close,segmented_model2)


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

df3down.scaled2=df3down.scaled[which(df3down.scaled$Time<0.6),]
df3down.scaled2$Time <- df3down.scaled2$Time/max(df3down.scaled2$Time)
segmented_model3.2 <- segmented(lm(Close ~ Time, df3down.scaled2), seg.Z = ~ Time, 
                              psi = c(quantile(df3down.scaled2$Time, c(0.25,0.75))))
#summary(segmented_model)
visualize_segmented_model(df3down.scaled2$Time,df3down.scaled2$Close,segmented_model3.2)



#----
term <- 15
seg.term = round(period1.down/term)
seg <- seq(0,1,length.out = (seg.term+2))[2:seg.term]

segmented_model1 <- segmented(lm(Close ~ Time, df1down.scaled), seg.Z = ~ Time,
                              psi = c(quantile(df1down.scaled$Time, seg)))
visualize_segmented_model(df1down.scaled$Time,df1down.scaled$Close,segmented_model1)

pred <- predict(segmented_model1)
breakpoints <- round(segmented_model1$psi[,'Est.']*period1.down,0)
breakpoints <- c(1,breakpoints,period1.down)
break.down1 <- pred[breakpoints]


seg.term = round(period2.down/term,0)
seg <- seq(0,1,length.out = (seg.term+2))[2:seg.term]
segmented_model2 <- segmented(lm(Close ~ Time, df2down.scaled), seg.Z = ~ Time, 
                              psi = c(quantile(df2down.scaled$Time, seg)))
visualize_segmented_model(df2down.scaled$Time,df2down.scaled$Close,segmented_model2)

pred <- predict(segmented_model2)
breakpoints <- round(segmented_model2$psi[,'Est.']*period2.down,0)
breakpoints <- c(1,breakpoints,period2.down)
break.down2 <- pred[breakpoints]


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

