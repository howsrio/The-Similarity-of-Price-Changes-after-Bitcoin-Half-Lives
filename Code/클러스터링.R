library('segmented')
library('dtw')
library(openxlsx)


BTC1.df <- dual.BTC1[1:MinIndex1.2,]
BTC2.df <- dual.BTC2[1:MinIndex2.2,]
BTC3.df <- dual.BTC3[1:MinIndex3.2,]
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


365*2.5
30*30

size <- sample(180:900,1)
start <- sample(1:(nrow(BTC)-size-1),1)
BTC$Close[(start):(start+size)]




#--------------

#BTC1 임의 추출 검정정

size.list <- c(MinIndex2.2,MinIndex3.2)
start.list <- c(TargetIndex2,TargetIndex3)
dist.list <- c(dtw12$distance,dtw13$distance)
norm.dist.list <- c(dtw12$normalizedDistance,dtw13$normalizedDistance)
rate.list <- c(0,0)
include.list <- c(0,0)


for (i in 1:100000){
  
  size <- sample(180:1460,1)
  start <- sample(1:(nrow(BTC)-size-1),1)
  sample <- BTC$Close[(start):(start+size)]
  
  rate <- length(intersect((start):(start+size),TargetIndex1:(TargetIndex1+MinIndex1.2)))/size
  include <- length(intersect((start):(start+size),TargetIndex1:(TargetIndex1+MinIndex1.2)))/MinIndex1.2
  
  sample.scaled <- sample / (max(sample)-min(sample))
  
  tryCatch({
    dtw.sam <- dtw(BTC1.scaled,sample.scaled)
    size.list <- c(size.list,size)
    start.list <- c(start.list, start)
    dist.list <- c(dist.list, dtw.sam$distance)
    norm.dist.list <- c(norm.dist.list,dtw.sam$normalizedDistance)
    rate.list <- c(rate.list,rate)
    include.list <- c(include.list,include)
    
  }, error = function(e) {
    # 예외가 발생했을 때 실행할 코드
    cat("An error occurred with size:",size,'start',start)
  })
  if (i/100 == 0) plot(dtw.sam)
  
}

compare.BTC1 <- cbind(size.list,start.list,dist.list,norm.dist.list,rate.list,include.list)
BTC1.0rate <- compare.BTC1[(compare.BTC1[,'rate.list'] < 0.2),]

hist(BTC1.0rate[,"norm.dist.list"])
BTC1.0rate[order(BTC1.0rate[,"norm.dist.list"]),]

compare.BTC1.df <- as.data.frame(compare.BTC1)
write.xlsx(compare.BTC1.df,"BTC1 test dataset.xlsx")




#-----BTC2

size.list <- c(MinIndex1.2,MinIndex3.2)
start.list <- c(TargetIndex1,TargetIndex3)
dist.list <- c(dtw21$distance,dtw23$distance)
norm.dist.list <- c(dtw21$normalizedDistance,dtw23$normalizedDistance)
rate.list <- c(0,0)
include.list <- c(0,0)


for (i in 1:100000){
  
  size <- sample(180:1460,1)
  start <- sample(1:(nrow(BTC)-size-1),1)
  sample <- BTC$Close[(start):(start+size)]
  
  rate <- length(intersect((start):(start+size),TargetIndex2:(TargetIndex2+MinIndex2.2)))/size
  include <- length(intersect((start):(start+size),TargetIndex2:(TargetIndex2+MinIndex2.2)))/MinIndex2.2
  
  sample.scaled <- sample / (max(sample)-min(sample))
  
  tryCatch({
    dtw.sam <- dtw(BTC2.scaled,sample.scaled)
    size.list <- c(size.list,size)
    start.list <- c(start.list, start)
    dist.list <- c(dist.list, dtw.sam$distance)
    norm.dist.list <- c(norm.dist.list,dtw.sam$normalizedDistance)
    rate.list <- c(rate.list,rate)
    include.list <- c(include.list,include)
    
  }, error = function(e) {
    # 예외가 발생했을 때 실행할 코드
    cat("An error occurred with size:",size,'start',start)
  })
  if (i/100 == 0) cat(i,'/100000')
}

compare.BTC2 <- cbind(size.list,start.list,dist.list,norm.dist.list,rate.list,include.list)
BTC2.0rate <- compare.BTC2[(compare.BTC2[,'rate.list'] < 0.2),]

hist(BTC2.0rate[,"norm.dist.list"])
BTC2.0rate[order(BTC2.0rate[,"norm.dist.list"]),]

compare.BTC2.df <- as.data.frame(compare.BTC2)
write.xlsx(compare.BTC2.df,"BTC2 test dataset.xlsx")


#-----BTC3

size.list <- c(MinIndex2.2,MinIndex3.2)
start.list <- c(TargetIndex2,TargetIndex3)
dist.list <- c(dtw31$distance,dtw32$distance)
norm.dist.list <- c(dtw31$normalizedDistance,dtw32$normalizedDistance)
rate.list <- c(0,0)
include.list <- c(0,0)


for (i in 1:100000){
  
  size <- sample(180:1460,1)
  start <- sample(1:(nrow(BTC)-size-1),1)
  sample <- BTC$Close[(start):(start+size)]
  
  rate <- length(intersect((start):(start+size),TargetIndex3:(TargetIndex3+MinIndex3.2)))/size
  include <- length(intersect((start):(start+size),TargetIndex3:(TargetIndex3+MinIndex3.2)))/MinIndex3.2
  
  sample.scaled <- sample / (max(sample)-min(sample))
  
  tryCatch({
    dtw.sam <- dtw(BTC3.scaled,sample.scaled)
    size.list <- c(size.list,size)
    start.list <- c(start.list, start)
    dist.list <- c(dist.list, dtw.sam$distance)
    norm.dist.list <- c(norm.dist.list,dtw.sam$normalizedDistance)
    rate.list <- c(rate.list,rate)
    include.list <- c(include.list,include)
    
  }, error = function(e) {
    # 예외가 발생했을 때 실행할 코드
    cat("An error occurred with size:",size,'start',start)
  })
  if (i%%1000 == 0) cat(i,'/100000\n')
}

compare.BTC3 <- cbind(size.list,start.list,dist.list,norm.dist.list,rate.list,include.list)
BTC3.0rate <- compare.BTC3[(compare.BTC3[,'rate.list'] < 0.2),]

hist(BTC3.0rate[,"norm.dist.list"])
BTC3.0rate[order(BTC3.0rate[,"norm.dist.list"]),]

compare.BTC3.df <- as.data.frame(compare.BTC3)
write.xlsx(compare.BTC3.df,"BTC3 test dataset.xlsx")
