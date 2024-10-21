library(readxl)
library(agricolae)


Musgrowth<- read.csv("C:/Users/lalma/Dropbox/Raccoon/Growth/RacoonGrowth_Mussel_Length_Width_041222.csv")
View(RacoonGrowth_Mussel_Length_Width_041222)
setwd("C:/Users/lalma/Dropbox/Raccoon/Growth")

aggregate(`Width.Month`~Sitedepth+Date+ Species, Musgrowth, mean)
aggregate(`Width.Month`~Sitedepth+Date+ Species, Musgrowth, sd)
aggregate(`Width.Month`~Sitedepth+Date+ Species, Musgrowth, length)


aggregate(`Length.Month`~Sitedepth+Date+ Species, Musgrowth, mean)
aggregate(`Length/Month`~Sitedepth+Date+ Species, Musgrowth, sd)
aggregate(`Length/Month`~Sitedepth+Date+ Species, Musgrowth, length)

aggregate(`Length.Month`~Date+ Species, Musgrowth, mean)
aggregate(`Width.Month`~Date+ Species, Musgrowth, mean)

aov<-aov(`Length.Month`~Date*Sitedepth, data=Musgrowth)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "Sitedepth"), group=TRUE)
p
plot(p, las=2)

aov<-aov(`Width.Month`~Date*Sitedepth, data=Musgrowth)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "Sitedepth"), group=TRUE)
p
plot(p, las=2)

T2<-subset(Musgrowth,Date=="T2")

aov<-aov(`Width.Month`~Sitedepth, data=T2)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Sitedepth"), group=TRUE)
p
plot(p, las=2)
