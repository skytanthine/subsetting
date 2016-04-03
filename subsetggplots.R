getwd()
setwd("C:/Users/Dominique/Documents/Dominique/Rstuff/timeseriesplots/newplots/PositiveModeLipids/TICapplied_3NormsNoDry")

respiration<-read.csv(file="SRSF25_35FullLipidsposmode.csv",header=TRUE,stringsAsFactors=FALSE)

str(respiration)

library(ggplot2) 
library(reshape)
library(plyr)



respiration$Time<-as.numeric(as.character(respiration$Time))


respiration<-data.frame(Genotype=factor(c("Genotype"),levels=c("Salinas","RNAi")),Temperature=factor(c("Temperature"),levels=c("25","35")),Treatment=factor(c("Treatment"),levels=c("ABA","agar")))

str(respiration)

#
respiration<-as.matrix(respiration)
respiration
colnames(respiration)

#
colnames(respiration)<-c("Genotype","Time","Temperature","Treatment")

#
meltrespiration<-melt(respiration,id="Time")

str(meltrespiration)


startCol<-6

#
endCol<-1098
while(startCol<endCol){
picName<-paste(colnames(meltrespiration[startCol]),".png", sep="")

#
png(picName)
print({

r=ggplot(data=respiration,aes(x=Time,y=respiration$X5.7_760.59)+
geom_boxplot(aes(fill=factor(Time)))+
scale_fill_manual(values=c("2"='white',"12"='white',"24"='white',"36"='white'))+
geom_jitter(position=position_jitter(width=0.01))+
scale_y_continuous(limits=c(0,(1.8*max(as.numeric(as.character(unlist(respiration[startCol])))))))+
scale_x_continuous(limits=c(0:40),breaks=c(2,12,24,36))+
stat_summary(aes(group=factor(Time)),fun.data="mean_cl_boot",colour="red")+
stat_smooth(aes(group=factor(1)),se=FALSE,method="loess")+
theme_bw()+
theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank()))

require(plyr)
dlply(respiration, .(cy1),function(x)p%+%x)
plots[1]

#
 })

startCol <- startCol+1
dev.off()

}


rm(meltrespiration)
rm(respiration)





#
respiration$Time<-as.numeric(as.character(respiration$Time))


#FINAL:
startCol <- 6
endCol <- 1098
while(startCol<endCol){
picName <- paste(colnames(respiration[startCol]),".png", sep="")

png(picName)
print({
ggplot(data=respiration,aes(x=Time,y=as.numeric(as.character(unlist(respiration[startCol])))))+geom_boxplot(aes(fill=factor(Time)))+
scale_fill_manual(values=c("0"='white',"2" = 'white', "12" = 'white', "24" = 'white', "36" = 'white'))+
geom_jitter(position = position_jitter(width=0.01))+
ggtitle(colnames<-names(respiration)[6:111])+
ylab("Intensity")+xlab("Hours of Imbibition")+theme(legend.position="none")+
#scale_y_continuous(limits=c(0,(1.8*max(respiration[startCol]))))+
scale_y_continuous(limits=c(0,(1.8*max(as.numeric(as.character(unlist(respiration[startCol])))))))+
scale_x_continuous(limits=c(0,40))+
stat_summary(aes(group=factor(Time)),fun.data="mean_cl_boot",colour="red")+
stat_smooth(aes(group=factor(1)),se=FALSE,method="loess")+
theme_bw()+
theme(legend.position="none")+
theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())
 })
#
startCol <- startCol+1
dev.off()

}

rm(meltrespiration)
rm(respiration)
