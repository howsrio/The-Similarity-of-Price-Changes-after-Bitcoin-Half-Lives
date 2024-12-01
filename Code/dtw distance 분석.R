#install.packages('dtw')
library(dtw)

#상승구간
up.ds12 <- dtw(df1up.scaled$Close,df2up.scaled$Close,keep=TRUE,step=asymmetric)
up.ds23 <- dtw(df2up.scaled$Close,df3up.scaled$Close,keep=TRUE,step=asymmetric)
up.ds31 <- dtw(df3up.scaled$Close,df1up.scaled$Close,keep=TRUE,step=asymmetric)

up.ds12$distance
up.ds23$distance
up.ds31$distance
plot(up.ds12)
cat(dtw12$distance,dtw12$normalizedDistance)

plot(up.ds23)


plot(up.ds31)


#감소구간
down.ds12 <- dtw(df1down.scaled$Close,df2down.scaled$Close,keep=TRUE,step=asymmetric)
down.ds23 <- dtw(df2down.scaled$Close,df3down.scaled$Close,keep=TRUE,step=asymmetric)
down.ds31 <- dtw(df3down.scaled$Close,df1down.scaled$Close,keep=TRUE,step=asymmetric)


down.ds12$distance
down.ds23$distance
down.ds31$distance
plot(down.ds12)
plot(down.ds23)
plot(down.ds31)

#-----
#segment 분석
seg.up12 <- dtw(break.up1,break.up2,,keep=TRUE,step=asymmetric)
seg.up23 <- dtw(break.up2,break.up3,,keep=TRUE,step=asymmetric)
seg.up31 <- dtw(break.up3,break.up1,,keep=TRUE,step=asymmetric)

seg.up12$distance
seg.up23$distance
seg.up31$distance

plot(seg.up12)
plot(seg.up23)
plot(seg.up31)


seg.down12 <- dtw(break.down1,break.down2,,keep=TRUE,step=asymmetric)
seg.down23 <- dtw(break.down2,break.down3,,keep=TRUE,step=asymmetric)
seg.down31 <- dtw(break.down3,break.down1,,keep=TRUE,step=asymmetric)

seg.down12$distance
seg.down23$distance
seg.down31$distance

plot(seg.down12)
plot(seg.down23)
plot(seg.down31)
