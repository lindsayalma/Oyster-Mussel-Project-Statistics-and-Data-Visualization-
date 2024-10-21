library(readxl)
  RaccoonHistology<- read_excel("C:/Users/lalma/Dropbox/Raccoon/Histology/Histology_Raccoon_012422.xlsx", 
                                         sheet = "Pearson's Chi oyster")
  library(readxl)
RaccoonHistology <- read_excel("Histology_Raccoon_012422.xlsx", 
                                         sheet = "Oyster sex_mat")
  View(RaccoonHistology)

View(RaccoonHistology)
oysteranova <- read_excel("Histology_Raccoon_012422.xlsx", 
                                       sheet = "Oyster ANOVA")

Histology_Raccoon_012422 <- read_excel("C:/Users/lalma/Dropbox/Raccoon/Histology/Histology_Raccoon_012422.xlsx", 
                                       sheet = "mussel bin")


RaccoonHistology$`num sex`

growth.aov<-aov(`Maturation stage`~sitedepth*date, data=oysteranova)
summary(growth.aov)
TukeyHSD(growth.aov)

growth.aov<-aov(`num sex`~sitedepth*date, data=RaccoonHistology)
summary(growth.aov)
TukeyHSD(growth.aov)

aggregate(`Maturation stage`~sitedepth+date, oysteranova, mean)


CT<-table(RaccoonHistology$sitedepth, RaccoonHistology$Rankname)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes
CT<-table(RaccoonHistology$date, RaccoonHistology$Rankname)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes

CT<-table(RaccoonHistology$sitedepth, RaccoonHistology$Sex)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes
CT<-table(RaccoonHistology$date, RaccoonHistology$Sex)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes

CT<-table(RaccoonHistology$sitedepth, RaccoonHistology$hermaphrodite)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes
CT<-table(RaccoonHistology$date, RaccoonHistology$hermaphrodite)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes

CT<-table(RaccoonHistology$sitedepth, RaccoonHistology$MorF)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes
CT<-table(RaccoonHistology$date, RaccoonHistology$MorF)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes

library(readxl)
RaccoonHistology <- read_excel("RaccoonHistology.xlsx", 
                               sheet = "Oyster Feret Egg Size")
View(RaccoonHistology)

aggregate(`Feret diameter (µm)`~Sitedepth+Date, RaccoonHistology, mean)
aggregate(`Feret diameter (µm)`~Sitedepth+Date, RaccoonHistology, sd)
aggregate(`Feret diameter (µm)`~Sitedepth+Date, RaccoonHistology, length)

##mussel chi
CT<-table(RaccoonHistology$Sitedepth, RaccoonHistologymussel$Rank)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes
CT<-table(RaccoonHistology$Date, RaccoonHistologymussel$Rank)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes

library(readxl)
RaccoonHistology <- read_excel("C:/Users/lalma/Dropbox/Raccoon/Histology/RaccoonHistology.xlsx", 
                               sheet = "Oyster sex ratio all HP")
View(RaccoonHistology)

unique(RaccoonHistology$unique_mat)
unique(RaccoonHistology$unique)
RaccoonHistology$

aggregate(sitedepth~mat, RaccoonHistology, length)
aggregate(sitedepth ~ unique, RaccoonHistology, length)



library(readxl)
RaccoonHistology <- read_excel("C:/Users/lalma/Dropbox/Raccoon/Histology/Histology_Raccoon_012422.xlsx", 
                                       sheet = "Mussel ANOVA")

#mussel stats
CT<-table(RaccoonHistology$Sitedepth, RaccoonHistology$Rank)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes
CT<-table(RaccoonHistology$Date, RaccoonHistology$Rank)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes

CT<-table(RaccoonHistology$Sitedepth, RaccoonHistology$Sex)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes
CT<-table(RaccoonHistology$Date, RaccoonHistology$Sex)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes

growth.aov<-aov(`Rank`~Sitedepth*Date, data=RaccoonHistology)
summary(growth.aov)
TukeyHSD(growth.aov)

growth.aov<-aov(`sex`~Sitedepth*date, data=RaccoonHistology)
summary(growth.aov)
TukeyHSD(growth.aov)

CT<-table(Histology_Raccoon_012422y$Sitedepth, Histology_Raccoon_012422$maturing)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes
CT<-table(RaccoonHistology$Date, RaccoonHistology$Rank)
chisq.test(CT, simulate.p.value = T, B = 10000)   #for sample sizes that are sufficiently high. If not, the chisq.test output will let you know. 
fisher.test(CT, simulate.p.value = T, B = 10000)  #for low sample sizes
