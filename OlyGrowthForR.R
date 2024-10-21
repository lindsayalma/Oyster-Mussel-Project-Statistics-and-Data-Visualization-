library(car)
library(readxl)
T1T2GrowthForR <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Growth/T1T2GrowthForR.xlsx", 
                             sheet = "GrowthDataForR")
View(T1T2GrowthForR)

T1T2GrowthForR <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Growth/T1T2GrowthForR.xlsx", 
                             sheet = "GrowthDataforR_mussel")
library(readxl)
mussel <- read_excel("RacoonGrowth_Mussel_Length_Width_041222.xlsx")
View(RacoonGrowth_Mussel_Length_Width_041222)


aggregate(`Width/Month`~Sitedepth+Date+ Species, T1T2GrowthForR, mean)
aggregate(`Width/Month`~Sitedepth+Date+ Species, T1T2GrowthForR, sd)
aggregate(`Width/Month`~Sitedepth+Date+ Species, T1T2GrowthForR, length)


aggregate(`Length/Month`~Sitedepth+Date+ Species, T1T2GrowthForR, mean)
aggregate(`Length/Month`~Sitedepth+Date+ Species, T1T2GrowthForR, sd)
aggregate(`Length/Month`~Sitedepth+Date+ Species, T1T2GrowthForR, length)



aggregate(`Length/Month`~Date+ Species, oyster, mean)
aggregate(`Width/Month`~Date+ Species, oyster, mean)

mussel<-subset(T1T2GrowthForR, Species=="M")
oyster<-subset(T1T2GrowthForR, Species=="O")

View(oyster)
View(mussel)

T1O<-subset(T1T2GrowthForR, date=="T1")
View(T1O)
T1O$Avemo
growth.aov<-aov(Avemo~ SiteDepth, data=T1O)
summary(growth.aov)
TukeyHSD(growth.aov)
mussel$`Width/Month`

aggregate(`Width/Month`~Sitedepth+Date, mussel, mean)
aggregate(`Width/Month`~Sitedepth+Date, mussel, sd)
aggregate(`Width/Month`~Sitedepth+Date, mussel, length)


growth.aov<-aov(`Length/Month`~Date, data=mussel)
summary(growth.aov)
TukeyHSD(growth.aov)
shapiro.test(resid(growth.aov))#not normal
leveneTest(`Length/Month`~Sitedepth*Date, data=mussel)##variances are equal, p>0.05, parametric test ok
loggrowth.aov<-aov(log`Length/Month`~Sitedepth*Date, data=mussel)#log transform
summary(loggrowth.aov)
TukeyHSD(loggrowth.aov)
shapiro.test(resid(loggrowth.aov))#not normals
sqgrowth.aov<-aov(sqrt`Length/Month`~Site, data=mussel)#sqrt transform
summary(sqgrowth.aov)
TukeyHSD(sqgrowth.aov)
#anova are robust against non-normality assumption

oyster<-na.omit(oyster)
growth.aov<-aov(`Length/Month`~Sitedepth*Date, data=oyster)
summary(growth.aov)
TukeyHSD(growth.aov)
shapiro.test(resid(growth.aov))#not normal
leveneTest(`Length/Month`~Sitedepth*Date, data=oyster)##variances are equal, p>0.05, parametric test ok
loggrowth.aov<-aov(log(`Length/Month`)~Sitedepth*Date, data=oyster)#log transform
summary(loggrowth.aov)
TukeyHSD(loggrowth.aov)
shapiro.test(resid(loggrowth.aov))#not normals
sqgrowth.aov<-aov(sqrt(`Length/Month`)~Sitedepth*Date, data=oyster)#sqrt transform
summary(sqgrowth.aov)
TukeyHSD(sqgrowth.aov)
shapiro.test(resid(sqgrowth.aov))#normally distributed

growth.aov<-aov(`Width/Month`~Sitedepth*Date, data=oyster)
summary(growth.aov)
TukeyHSD(growth.aov)

growth.aov<-aov(`Length/Month`~Sitedepth*Date, data=oyster)
summary(growth.aov)
TukeyHSD(growth.aov)

growth.aov<-aov(`Length/Month`~Sitedepth*Date, data=mussel)
summary(growth.aov)
TukeyHSD(growth.aov)

growth.aov<-aov(`Width/Month`~Sitedepth*Date, data=mussel)
summary(growth.aov)
TukeyHSD(growth.aov)

head(mussel)

boxplot(`Length/Month`~ Date*Sitedepth, data=mussel, col=c("green","blue" ,"red","yellow"),ylab="Length growth/month (mm)")
boxplot(`Width/Month`~ Date*Sitedepth, data=mussel, col=c( "green","blue" ,"red","yellow"),  ylab="Width growth/month (mm)")


aggregate(`Length/Month`~Date, oyster, mean)
aggregate(`Width/Month`~Date, oyster, mean)

aggregate(`Length/Month`~Date, mussel, mean)
aggregate(`Width/Month`~Date, mussel, mean)