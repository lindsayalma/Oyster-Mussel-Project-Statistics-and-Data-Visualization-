library(readxl)
library(emmeans)
library(rlang)
library(car)

T1T2SSforR <- read_excel("C:/Users/lalma/Dropbox/Raccoon/Shell Strength/T1T2SSforR.xlsx")
View(T1T2SSforR)

MusselO<-subset(T1T2SSforR, Species=="M" & Hole=="O")
MusselI<-subset(T1T2SSforR, Species=="M" & Hole=="I")
OysterO<-subset(T1T2SSforR, Species=="O" & Hole== "O")
OysterI<-subset(T1T2SSforR, Species=="O" & Hole== "I")
Mussel<-subset(T1T2SSforR, Species=="M")
Oyster<-subset(T1T2SSforR, Species=="O")

###MUSSEL

aov<-aov(Force~Site*Date, data=Mussel)
summary(aov)
TukeyHSD(aov)
aggregate(Force~Site+Date, data=Mussel, mean)
aggregate(Force~Site+Date, data=Mussel, sd)
aggregate(Force~Site+Date, data=Mussel, length)
shapiro.test(resid(aov))#not normal p<0.05
leveneTest(Force ~ Site*Date, data = Mussel)#not equal variance p<0.05

aggregate(Force~Date, data=Mussel, mean)

###Oyster
aov<-aov(Force~Site*Date, data=Oyster)
summary(aov)
TukeyHSD(aov)
aggregate(Force~Site+Date, data=Oyster, mean)
aggregate(Force~Site+Date, data=Oyster, sd)
aggregate(Force~Site+Date, data=Oyster, length)
shapiro.test(resid(aov))#not normal p<0.05
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



T1T2SSforR_ave<-aggregate(Force~Site+Date+ Species+ID, T1T2SSforR, mean)

write.csv(T1T2SSforR_ave, file = "C:/Users/Lindsay/Dropbox/Raccoon/Correlations/T1T2SS_ave.csv", row.names = T)

aggregate(Force~Date, data=Oyster, mean)


