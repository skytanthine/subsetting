getwd()
setwd("C:/Users/Dominique/Documents/Dominique/Rstuff/timeseriesplots/newplots/PositiveModeLipids/TICapplied_3NormsNoDry")

respiration<-read.csv(file="SRSF25_35FullLipidsposmode.csv",header=TRUE,stringsAsFactors=FALSE)

str(respiration)

library(ggplot2) 
library(reshape)
library(plyr)

respiration$Time<-as.numeric(as.character(respiration$Time))

str(respiration)

startCol<-6

r=ggplot(data=respiration,aes(x=Time,y=respiration$X5.7_760.59)
geom_boxplot(aes(fill=factor(Time)))+
scale_fill_manual(values=c("2"='white',"12"='white',"24"='white',"36"='white'))
geom_jitter(position=position_jitter(width=0.01))
scale_y_continuous(limits=c(0,(1.8*max(as.numeric(as.character(unlist(respiration[startCol])))))))
scale_x_continuous(limits=c(0:40),breaks=c(2,12,24,36))
stat_summary(aes(group=factor(Time)),fun.data="mean_cl_boot",colour="red")
stat_smooth(aes(group=factor(1)),se=FALSE,method="loess")
theme_bw()
theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())

require(plyr)
options(dplyr.width=Inf)
dlply(respiration,.(X5.7_760.59),function(x)%+%x)

rm(respiration)





