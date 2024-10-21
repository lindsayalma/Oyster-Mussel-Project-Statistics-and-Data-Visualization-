library(readxl)
SS <- read_excel("C:/Users/Lindsay Alma/Dropbox/Raccoon/Correlations/SScorrGrowth.xlsx")

SS$`L Growth/mo`
aov<-aov(Force~Sitedepth*Date+`L Growth/mo`, data=SS)
summary(aov)


lipid<- read.csv("C:/Users/Lindsay Alma/Dropbox/Raccoon/Correlations/LipidsCorrGrowth_111423.csv")

lipid$L.month
aov<-aov(X.lipid~Sitedepth*Date.x + L.month, data=lipid)
summary(aov)


N<- read_excel("C:/Users/Lindsay Alma/Dropbox/Raccoon/Correlations/IsotopesCorrGrowth_111423.xlsx")
N <- na.omit(N)
ON<-subset(N, Species=="O")
N$Date.x
aov<-aov(N~Sitedepth*Date + LM, data=ON)
summary(aov)


C<- read_excel("C:/Users/Lindsay Alma/Dropbox/Raccoon/Correlations/IsotopesCorrGrowth_111423.xlsx")
C <- na.omit(C)
OC<-subset(C, Species=="O")
N$Date.x
aov<-aov(C~Sitedepth*Date + LM, data=OC)
summary(aov)


