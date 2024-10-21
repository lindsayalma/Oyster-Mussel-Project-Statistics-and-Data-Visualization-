library(readxl)
library(emmeans)
library(rlang)
library(car)
library(agricolae)

setwd("C:/Users/lalma/Dropbox/Raccoon/Lipids and Fatty Acids")

RaccoonLipids <- read_excel("C:/Users/lalma/Dropbox/Raccoon/Lipids and Fatty Acids/RaccoonLipids.xlsx")
View(RaccoonLipids)

Mussel<-subset(RaccoonLipids, Species=="M")
Oyster<-subset(RaccoonLipids, Species=="O")

RaccoonLipids$`%lipid`


aggregate(`%lipid`~Sitedepthspecies, data=Mussel, mean)
aggregate(`%lipid`~Sitedepthspecies, data=Mussel, sd)
aggregate(`%lipid`~Sitedepthspecies, data=Mussel, length)

aggregate(`%lipid`~Sitedepthspecies, data=Oyster, mean)
aggregate(`%lipid`~Sitedepthspecies, data=Oyster, sd)
aggregate(`%lipid`~Sitedepthspecies, data=Oyster, length)


###MUSSEL
aov<-aov(`%lipid`~Site*Date, data=Mussel)
summary(aov)
TukeyHSD(aov)
shapiro.test(resid(aov))#not normal p<0.05
leveneTest(Force ~ Site*Date, data = Mussel)#not equal variance p<0.05
p<-HSD.test(aov, c("Sitedepthspecies"), group=TRUE)
p
plot(p, las=2)

###Oyster
aov<-aov(`%lipid`~Site*Date, data=Oyster)
summary(aov)
TukeyHSD(aov)

leveneTest(Force ~ Site*Date, data = Oyster)#not equal variance p<0.05

library(readxl)
Outter_inner <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Shell Strength/Outter-inner.xlsx")
View(Outter_inner)
MusselOI<-subset(Outter_inner, Species=="M")
OysterOI<-subset(Outter_inner, Species=="O")

###MUSSEL
aov<-aov(`Outter-Inner`~Site*Date, data=MusselOI)
summary(aov)
TukeyHSD(aov)
aggregate(`Outter-Inner`~Site+Date, data=MusselOI, mean)

###MUSSEL
aov<-aov(`Outter-Inner`~Site*Date, data=OysterOI)
summary(aov)
TukeyHSD(aov)
aggregate(`Outter-Inner`~Site+Date, data=OysterOI, mean)

aggregate(`%lipid`~Date, data=Oyster, mean)
