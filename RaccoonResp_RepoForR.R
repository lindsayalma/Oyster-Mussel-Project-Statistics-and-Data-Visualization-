library(readxl)
library(agricolae)

setwd("C:/Users/Lindsay Alma/Dropbox/Raccoon/Respiration")
resp_repod_081423 <- read_excel("resp_repod_081423.xlsx")


#T1=dabob has more females – also has the highest metabolic rate at hig temps


#T1:
 # to has 80 mature 20 post-
  
#ci20 20 early 80 post
#ci5 80 mature 20 post (same as To)
#db 50 matire 50 post
#pw 30 mature 70 post

#T2:
 # Ci20: 50 mature 50 post
#Ci5 80 mature 20 post
#Db 100 post
#Pw 90 mature 10 post






M<-subset(Resp, Species=="M")
O<-subset(Resp, Species=="O")

View(O)


Oyster_Temp<-subset(O, Stressor=="T")
Oyster_pH<-subset(O, Stressor=="P")

Mussel_Temp<-subset(M, Stressor=="T")
Mussel_pH<-subset(M, Stressor=="P")


Oyster_Temp_CI20<-subset(Oyster_Temp, sitedepth=="CI20")
Oyster_Temp_CI5<-subset(Oyster_Temp, sitedepth=="CI5")
Oyster_Temp_DB<-subset(Oyster_Temp, sitedepth=="DB5")
Oyster_Temp_PW<-subset(Oyster_Temp, sitedepth=="PW5")
Oyster_pH_CI20<-subset(Oyster_pH, sitedepth=="CI20")
Oyster_pH_CI5<-subset(Oyster_pH, sitedepth=="CI5")
Oyster_pH_DB<-subset(Oyster_pH, sitedepth=="DB5")
Oyster_pH_PW<-subset(Oyster_pH, sitedepth=="PW5")



Mussel_Temp_CI20<-subset(Mussel_Temp, sitedepth=="CI20")
Mussel_Temp_CI5<-subset(Mussel_Temp, sitedepth=="CI5")
Mussel_Temp_DB<-subset(Mussel_Temp, sitedepth=="DB5")
Mussel_Temp_PW<-subset(Mussel_Temp, sitedepth=="PW5")
Mussel_pH_CI20<-subset(Mussel_pH, sitedepth=="CI20")
Mussel_pH_CI5<-subset(Mussel_pH, sitedepth=="CI5")
Mussel_pH_DB<-subset(Mussel_pH, sitedepth=="DB5")
Mussel_pH_PW<-subset(Mussel_pH, sitedepth=="PW5")





Oyster_Temp_CI20_T1<-subset(Oyster_Temp_CI20, Date=="T1")
Oyster_Temp_CI5_T1<-subset(Oyster_Temp_CI5, Date=="T1")
Oyster_Temp_DB_T1<-subset(Oyster_Temp_DB, Date=="T1")
Oyster_Temp_PW_T1<-subset(Oyster_Temp_PW, Date=="T1")

Oyster_Temp_CI20_T2<-subset(Oyster_Temp_CI20, Date=="T2")
Oyster_Temp_CI5_T2<-subset(Oyster_Temp_CI5, Date=="T2")
Oyster_Temp_DB_T2<-subset(Oyster_Temp_DB, Date=="T2")
Oyster_Temp_PW_T2<-subset(Oyster_Temp_PW, Date=="T2")

Oyster_pH_CI20_T1<-subset(Oyster_pH_CI20, Date=="T1")
Oyster_pH_CI5_T1<-subset(Oyster_pH_CI5, Date=="T1")
Oyster_pH_DB_T1<-subset(Oyster_pH_DB, Date=="T1")
Oyster_pH_PW_T1<-subset(Oyster_pH_PW, Date=="T1")

Oyster_pH_CI20_T2<-subset(Oyster_pH_CI20, Date=="T2")
Oyster_pH_CI5_T2<-subset(Oyster_pH_CI5, Date=="T2")
Oyster_pH_DB_T2<-subset(Oyster_pH_DB, Date=="T2")
Oyster_pH_PW_T2<-subset(Oyster_pH_PW, Date=="T2")


aggregate(o2consumption~sitedepth+Date+Treatment, data=Mussel_Temp, mean)
aggregate(o2consumption~sitedepth+Date+Treatment, data=Mussel_Temp, sd)
aggregate(o2consumption~sitedepth+Date+Treatment, data=Mussel_Temp, length)




aggregate(o2consumption~sitedepth+Date+Treatment, data=Mussel_pH, mean)
aggregate(o2consumption~sitedepth+Date+Treatment, data=Mussel_pH, sd)
aggregate(o2consumption~sitedepth+Date+Treatment, data=Mussel_pH, length)



aggregate(o2consumption~sitedepth+Date+Treatment, data=Oyster_pH, mean)
aggregate(o2consumption~sitedepth+Date+Treatment, data=Oyster_pH, sd)
aggregate(o2consumption~sitedepth+Date+Treatment, data=Oyster_pH, length)



Oyster_Temp_3C<-subset(Oyster_Temp, Treatment=="03C")
Oyster_Temp_8.5C<-subset(Oyster_Temp, Treatment=="08.5C")
Oyster_Temp_14C<-subset(Oyster_Temp, Treatment=="14C")
Oyster_Temp_19.5C<-subset(Oyster_Temp, Treatment=="19.5C")
Oyster_Temp_25C<-subset(Oyster_Temp, Treatment=="26C")

Oyster_T1_Temp<-subset(Oyster_Temp, Date=="T1")
Oyster_T2_Temp<-subset(Oyster_Temp, Date=="T2")
Oyster_T1_pH<-subset(Oyster_pH, Date=="T1")
Oyster_T2_pH<-subset(Oyster_pH, Date=="T2")

Oyster_pH_6.6<-subset(Oyster_pH, Treatment=="pH6.6")
Oyster_pH_6.9<-subset(Oyster_pH, Treatment=="pH6.9")
Oyster_pH_7.2<-subset(Oyster_pH, Treatment=="pH7.2")
Oyster_pH_7.5<-subset(Oyster_pH, Treatment=="pH7.5")
Oyster_pH_7.8<-subset(Oyster_pH, Treatment=="pH7.8")


View(O)
T1CI20<-subset(resp_repod_081423, sitedepth=="CI20")
T1CI20<-subset(T1CI20, ate=="T1")
#
# Three Way ANOVA

aov<-aov(consumption~Early, data=T1CI20)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Treatment" , "Date", "sitedepth"), group=TRUE)
p
plot(p)

aov<-aov(o2consumption~sitedepth, data=Oyster_Temp)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c(  "sitedepth"), group=TRUE)
par(cex = 0.5) 
p
plot(p)


aov<-aovaov<-aov(o2consumption~sitedepth*Date*Treatment, data=Oyster_pH)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Date", "sitedepth", "Treatment"), group=TRUE)
p
plot(p,las=2)



aov<-aov(o2consumption~sitedepth*Date*Treatment, data=Mussel_Temp)
summary(aov)
TukeyHSD(aov)

aov<-aov(o2consumption~sitedepth*Date*Treatment, data=Mussel_pH)
summary(aov)
TukeyHSD(aov)




######Mussel  2 way anova with individual sites###########################
#########################temp########################
aov<-aov(o2consumption~Date*Treatment, data=Mussel_Temp_CI20)
summary(aov)
TukeyHSD(aov)


aov<-aov(o2consumption~Date*Treatment, data=Mussel_Temp_CI5)
summary(aov)
TukeyHSD(aov)

aov<-aov(o2consumption~Date*Treatment, data=Mussel_Temp_DB)
summary(aov)
TukeyHSD(aov)

aov<-aov(o2consumption~Date*Treatment, data=Mussel_Temp_PW)
summary(aov)
TukeyHSD(aov)

View(Mussel_Temp_DB)

######oyster  2 way anova with individual sites###########################
#########################temp########################

aov<-aov(o2consumption~Date*Treatment, data=Oyster_Temp_CI20)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Date",  "Treatment"), group=TRUE)
p
plot(p,las=2)

aov<-aov(o2consumption~Date*Treatment, data=Oyster_Temp_DB)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Date",  "Treatment"), group=TRUE)
p
plot(p,las=2)

aov<-aov(o2consumption~Date*Treatment, data=Oyster_Temp_PW)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Date",  "Treatment"), group=TRUE)
p
plot(p,las=2)


aov<-aov(o2consumption~Date*Treatment, data=Oyster_Temp_CI5)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Date",  "Treatment"), group=TRUE)
p
plot(p,las=2)

####################### Mussel 2 way anova with individual sites###############
##############################pH####################################
aov<-aov(o2consumption~Date*Treatment, data=Mussel_pH_CI20)
summary(aov)
TukeyHSD(aov)

aov<-aov(o2consumption~Date*Treatment, data=Mussel_pH_CI5)
summary(aov)
TukeyHSD(aov)

aov<-aov(o2consumption~Date*Treatment, data=Mussel_pH_DB)
summary(aov)
TukeyHSD(aov)

aov<-aov(o2consumption~Date*Treatment, data=Mussel_pH_PW)
summary(aov)
TukeyHSD(aov)

#######################Oyster 2 way anova with individual sites###############
##############################pH####################################

aov<-aov(o2consumption~Date*Treatment, data=Oyster_pH_CI20)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Date",  "Treatment"), group=TRUE)
p
plot(p,las=2)

aov<-aov(o2consumption~Date*Treatment, data=Oyster_pH_CI5)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Date",  "Treatment"), group=TRUE)
p
plot(p,las=2)

aov<-aov(o2consumption~Date*Treatment, data=Oyster_pH_DB)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Date",  "Treatment"), group=TRUE)
p
plot(p,las=2)

aov<-aov(o2consumption~Date*Treatment, data=Oyster_pH_PW)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Date",  "Treatment"), group=TRUE)
p
plot(p,las=2)

#########2 way anova compare sites within same date######################
##################oyster###################

aov<-aov(o2consumption~sitedepth*Treatment, data=Oyster_T1_Temp)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("sitedepth",  "Treatment"), group=TRUE)
p
plot(p,las=2)

aov<-aov(o2consumption~sitedepth*Treatment, data=Oyster_T2_Temp)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("sitedepth",  "Treatment"), group=TRUE)
p
plot(p,las=2)

aov<-aov(o2consumption~sitedepth*Treatment, data=Oyster_T1_pH)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("sitedepth",  "Treatment"), group=TRUE)
p
plot(p,las=2)

aov<-aov(o2consumption~sitedepth*Treatment, data=Oyster_T2_pH)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("sitedepth",  "Treatment"), group=TRUE)
p
plot(p,las=2)


######################## 1 way anova sinlgle sites and dates OYSTER###############################

aov<-aov(o2consumption~Treatment, data=Oyster_Temp_CI20_T1)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Treatment"), group=TRUE)
p
plot(p,las=2)

aov<-aov(o2consumption~Treatment, data=Oyster_Temp_CI5_T1)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, "Treatment", group=TRUE)
p

aov<-aov(o2consumption~Treatment, data=Oyster_Temp_DB_T1)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, "Treatment", group=TRUE)
p

aov<-aov(o2consumption~Treatment, data=Oyster_Temp_PW_T1)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, "Treatment", group=TRUE)
p

aov<-aov(o2consumption~Treatment, data=Oyster_Temp_CI20_T1)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, "Treatment", group=TRUE)
p

aov<-aov(o2consumption~Treatment, data=Oyster_Temp_CI20_T2)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, "Treatment", group=TRUE)
p

aov<-aov(o2consumption~Treatment, data=Oyster_Temp_CI5_T2)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, "Treatment", group=TRUE)
p


aov<-aov(o2consumption~Treatment, data=Oyster_Temp_DB_T2)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, "Treatment", group=TRUE)
p

aov<-aov(o2consumption~Treatment, data=Oyster_Temp_PW_T2)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, "Treatment", group=TRUE)
p
View(Oyster_pH_CI20_T1)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_CI20_T1)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "sitedepth"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_CI5_T1)
summary(aov)
TukeyHSD(aov)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_DB_T1)
summary(aov)
TukeyHSD(aov)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_PW_T1)
summary(aov)
TukeyHSD(aov)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_CI20_T2)
summary(aov)
TukeyHSD(aov)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_CI5_T2)
summary(aov)
TukeyHSD(aov)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_DB_T2)
summary(aov)
TukeyHSD(aov)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_PW_T2)
summary(aov)
TukeyHSD(aov)


##########ANOVA specific treatments OYSTER##############
#########################temp###############

aov<-aov(o2consumption~sitedepth*Date, data=Oyster_Temp_3C)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "sitedepth"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~sitedepth*Date, data=Oyster_Temp_8.5C)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "sitedepth"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~sitedepth*Date, data=Oyster_Temp_14C)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "sitedepth"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~sitedepth*Date, data=Oyster_Temp_19.5C)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "sitedepth"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~sitedepth*Date, data=Oyster_Temp_25C)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "sitedepth"), group=TRUE)
p
plot(p, las=2)


##########ANOVA specific treatments OYSTER##############
#########################pH###############
aov<-aov(o2consumption~sitedepth*Date, data=Oyster_pH_6.6)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "sitedepth"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~sitedepth*Date, data=Oyster_pH_6.9)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "sitedepth"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~sitedepth*Date, data=Oyster_pH_7.2)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "sitedepth"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~sitedepth*Date, data=Oyster_pH_7.5)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "sitedepth"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~sitedepth*Date, data=Oyster_pH_7.8)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "sitedepth"), group=TRUE)
p
plot(p, las=2)


##########ANOVA specific sites and dates OYSTER##############
#########################pH###############

aov<-aov(o2consumption~Treatment, data=Oyster_pH_CI20_T1)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Treatment"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_CI20_T2)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Treatment"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_CI5_T1)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Treatment"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_CI5_T2)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Treatment"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_DB_T1)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Treatment"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_DB_T2)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Treatment"), group=TRUE)
p
plot(p, las=2)


aov<-aov(o2consumption~Treatment, data=Oyster_pH_PW_T1)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Treatment"), group=TRUE)
p
plot(p, las=2)

aov<-aov(o2consumption~Treatment, data=Oyster_pH_PW_T2)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Treatment"), group=TRUE)
p
plot(p, las=2)
