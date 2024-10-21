library(readxl)
library(writexl)
library(date)

setwd("C:/Users/lalma/Dropbox/Raccoon/Ocean")

CI20<- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/CI20forR.xlsx")

CI5<- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/CI5forR.xlsx")

PW<- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/PWforR.xlsx")

DB<- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/DBforR.xlsx")

#to calculate monthly averages

Raccoon_month_average_010122 <- read_excel("Raccoon_month_average_010122.xlsx")
View(Raccoon_month_average_010122)
m<-Raccoon_month_average_010122

AllDat<-rbind(DB,PW,CI5,CI20)


AllDat$date<-as.Date(AllDat$date,format="%Y-%m-%d")
stringsAsFactors=FALSE
AllDat$temp <- as.numeric(as.character(AllDat$temp))
AllDat$sal <- as.numeric(as.character(AllDat$sal))
AllDat$chla <- as.numeric(as.character(AllDat$chla))
ggplot(aes(x=date, y=sal, color=location),data=AllDat)+geom_line()+geom_point()+theme(text = element_text(size = 20))   +ylim(27,31)   
ggplot(aes(x=date, y=temp, color=location),data=AllDat)+geom_line() +theme(text = element_text(size = 20))   +ylim(7,16)   
ggplot(aes(x=date, y=do, color=location),data=AllDat)+geom_line() +theme(text = element_text(size = 20))   +ylim(4,12)   
ggplot(aes(x=date, y=chla, color=location),data=AllDat)+geom_line()+theme(text = element_text(size = 20))   +ylim(0,31)   


library(readxl)
CI5forR <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/CI5forR.xlsx")
View(CI5forR)

aggregate(temp ~ date, CI5forR, mean)
aggregate(sal ~ date, CI5forR, mean)
aggregate(do ~ date, CI5forR, mean)
aggregate(chla ~ date, CI5forR, mean)

library(readxl)
CI20forR <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/CI20forR.xlsx")
View(CI20forR)

aggregate(temp~ date, CI20forR, mean)
aggregate(sal ~ date, CI20forR, mean)
aggregate(do ~ date, CI20forR, mean)
aggregate(chla ~ date, CI20forR, mean)

library(readxl)
DBforR <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/DBforR.xlsx")
View(DBforR)

aggregate(temp~ date, DBforR, mean)
aggregate(sal ~ date, DBforR, mean)
aggregate(do ~ date, DBforR, mean)
aggregate(chla ~ date, DBforR, mean)

library(readxl)
DBforR <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/PWforR.xlsx")
View(PWforR)

aggregate(temp~ date, PWforR, mean)
aggregate(sal ~ date, PWforR, mean)
aggregate(do ~ date, PWforR, mean)
aggregate(chla ~ date, PWforR, mean)


# library to read matlab data formats into R
library(R.matlab)
CIpH<-readMat("C:/Users/Lindsay/Downloads/CI_PH_data.mat")

View(CIpH)
write.csv(CIpH)

write.csv(CIpH, file = "C:/Users/Lindsay/Downloads/CI_PH_data.csv", row.names = FALSE)

library(readr)
CI_PH_data <- read_csv("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/CI_PH_data.csv")
View(CI_PH_data)

aggregate(intPH~ date, CI_PH_data, mean)



#correlation matrix
install.packages("Hmisc")
library(readxl)
CorrMatrixForR <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/CorrMatrixForR.xlsx")
View(CorrMatrixForR)

library(readxl)
CorrMatrixForR_CI <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/CorrMatrixForR_CI.xlsx")
View(CorrMatrixForR_CI)

library(readxl)
CorrMatrixForR_DB <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/CorrMatrixForR_DB.xlsx")
View(CorrMatrixForR_DB)

library(readxl)
CorrMatrixForR_PW <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/CorrMatrixForR_PW.xlsx")
View(CorrMatrixForR_PW)

library(readxl)
CorrMatrixForR_CI5 <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/CorrMatrixForR_CI5.xlsx")
View(CorrMatrixForR_CI5)

install.packages("corrplot")
library(corrplot)

M<-CorrMatrixForR_CI20

M<-cor(M, use = "complete.obs")
head(round(M,2))

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(CorrMatrixForR)
head(p.mat[, 1:5])
#View(M)

#View(p.mat)
corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.05, insig = "blank")


####redo averages and sd with different database type##########
library(readxl)

o<-OCEAN_main_111121 <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/OCEAN_main_111121.xlsx", 
                                sheet = "OcenBoxplotForR")
head(o)

aggregate(Temp~ Sitedate, o, mean)
aggregate(Sal ~ Sitedate, o, mean)
aggregate(DO ~ Sitedate, o, mean)
aggregate(Chla ~ Sitedate, o, mean)
aggregate(ara ~ Sitedate, o, mean)
aggregate(pH ~ Sitedate, o, mean)
aggregate(pCO2 ~ Sitedate, o, mean)

aggregate(Temp~ Sitedate, o, mean)
aggregate(Sal ~ Sitedate, o, mean)
aggregate(DO ~ Sitedate, o, mean)
aggregate(Chla ~ Sitedate, o, mean)
aggregate(ara ~ Sitedate, o, mean)
aggregate(pH ~ Sitedate, o, mean)
aggregate(pCO2 ~ Sitedate, o, mean)

aggregate(Temp~ Sitedate, o, sd)
aggregate(Sal ~ Sitedate, o, sd)
aggregate(DO ~ Sitedate, o, sd)
aggregate(Chla ~ Sitedate, o, sd)
aggregate(ara ~ Sitedate, o, sd)
aggregate(pH ~ Sitedate, o, sd)
aggregate(pCO2 ~ Sitedate, o, sd)

aggregate(Temp~ Sitedate, o, length)
aggregate(Sal ~ Sitedate, o, length)
aggregate(DO ~ Sitedate, o, length)
aggregate(Chla ~ Sitedate, o, length)
aggregate(ara ~ Sitedate, o, length)
aggregate(pH ~ Sitedate, o, length)
aggregate(pCO2 ~ Sitedate, o, length)

aggregate(pCO2 ~ location, o, mean)



#averaging months data
m1<-aggregate(Temp~ dateID, m, mean)
m2<-aggregate(Sal ~ dateID, m, mean)
m3<-aggregate(DO ~ dateID, m, mean)
m4<-aggregate(Chla ~ dateID, m, mean)
m5<-aggregate(ara ~ dateID, m, mean)
m6<-aggregate(pH ~ dateID, m, mean)
m7<-aggregate(pCO2 ~ dateID, m, mean)
m1
M<-data.frame(c(m1,m2,m3,m4,m5,m6,m7))
write_xlsx(M, "C:/Users/lalma/Dropbox/Raccoon/Ocean/Raccoon_month_average.xlsx")
  
#averaging 5 ave months data
m1<-aggregate(Temp~ fiveS, m, mean)
m2<-aggregate(Sal ~ fiveS, m, mean)
m3<-aggregate(DO ~ fiveS, m, mean)
m4<-aggregate(Chla ~ fiveS, m, mean)
m5<-aggregate(ara ~ fiveS, m, mean)
m6<-aggregate(pH ~ fiveS, m, mean)
m7<-aggregate(pCO2 ~ fiveS, m, mean)

M<-data.frame(c(m1,m2,m3,m4,m5,m6,m7))
write_xlsx(M, "C:/Users/lalma/Dropbox/Raccoon/Ocean/Raccoon_5_ave.xlsx")

library(readxl)
OCEAN_main_111121 <- read_excel("C:/Users/lalma/Dropbox/Raccoon/Ocean/OCEAN_main_111121.xlsx")
View(OCEAN_main_111121)


aov<-aov(Temp~location*date, data=OCEAN_main_111121)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c("Sitedepth"), group=TRUE)
p
plot(p, las=2)



quantile(OCEAN_main_111121$Temp, c(0.25, 0.5, 0.75))
quantile(OCEAN_main_111121$Sal, c(0.25, 0.5, 0.75))
quantile(OCEAN_main_111121$Chla, c(0.25, 0.5, 0.75))
quantile(OCEAN_main_111121$DO, c(0.25, 0.5, 0.75))
quantile(OCEAN_main_111121$pH, c(0.25, 0.5, 0.75))
quantile(OCEAN_main_111121$ara, c(0.25, 0.5, 0.75))

T1CI20<-subset(OCEAN_main_111121, sitedate=="T1CI20")
T1CI5<-subset(OCEAN_main_111121, sitedate=="T1CI5")
T1DB<-subset(OCEAN_main_111121, sitedate=="T1DB")
T1PW<-subset(OCEAN_main_111121, sitedate=="T1PW")
T2CI20<-subset(OCEAN_main_111121, sitedate=="T2CI20")
T2CI5<-subset(OCEAN_main_111121, sitedate=="T2CI5")
T2DB<-subset(OCEAN_main_111121, sitedate=="T2DB")
T2PW<-subset(OCEAN_main_111121, sitedate=="T2PW")

#temp
sum(T1CI20$Temp < 9.1, na.rm=TRUE)
sum(T1CI5$Temp < 9.1, na.rm=TRUE)
sum(T1DB$Temp < 9.1, na.rm=TRUE)
sum(T1PW$Temp < 9.1, na.rm=TRUE)

sum(T1CI20$Temp >13.2, na.rm=TRUE)
sum(T1CI5$Temp >13.2, na.rm=TRUE)
sum(T1DB$Temp >13.2, na.rm=TRUE)
sum(T1PW$Temp >13.2, na.rm=TRUE)

sum(T2CI20$Temp < 9.1, na.rm=TRUE)
sum(T2CI5$Temp < 9.1, na.rm=TRUE)
sum(T2DB$Temp < 9.1, na.rm=TRUE)
sum(T2PW$Temp < 9.1, na.rm=TRUE)

sum(T2CI20$Temp >13.2, na.rm=TRUE)
sum(T2CI5$Temp >13.2, na.rm=TRUE)
sum(T2DB$Temp >13.2, na.rm=TRUE)
sum(T2PW$Temp >13.2, na.rm=TRUE)


#sal
sum(T1CI20$Sal < 29.26, na.rm=TRUE)
sum(T1CI5$Sal < 29.26, na.rm=TRUE)
sum(T1DB$Sal < 29.26, na.rm=TRUE)
sum(T1PW$Sal < 29.26, na.rm=TRUE)

sum(T1CI20$Sal >30.3, na.rm=TRUE)
sum(T1CI5$Sal >30.3, na.rm=TRUE)
sum(T1DB$Sal >30.3, na.rm=TRUE)
sum(T1PW$Sal >30.3, na.rm=TRUE)

sum(T2CI20$Sal < 29.26, na.rm=TRUE)
sum(T2CI5$Sal < 29.26, na.rm=TRUE)
sum(T2DB$Sal < 29.26, na.rm=TRUE)
sum(T2PW$Sal < 29.26, na.rm=TRUE)

sum(T2CI20$Sal >30.3, na.rm=TRUE)
sum(T2CI5$Sal >30.3, na.rm=TRUE)
sum(T2DB$Sal >30.3, na.rm=TRUE)
sum(T2PW$Sal >30.3, na.rm=TRUE)


#chla
sum(T1CI20$Chla >3.5, na.rm=TRUE)
sum(T1CI5$Chla >3.5, na.rm=TRUE)
sum(T1DB$Chla >3.5, na.rm=TRUE)
sum(T1PW$Chla >3.5, na.rm=TRUE)

sum(T2CI20$Chla >3.5, na.rm=TRUE)
sum(T2CI5$Chla >3.5, na.rm=TRUE)
sum(T2DB$Chla >3.5, na.rm=TRUE)
sum(T2PW$Chla >3.5, na.rm=TRUE)

sum(T1CI20$Chla < 1.2, na.rm=TRUE)
sum(T1CI5$Chla < 1.2, na.rm=TRUE)
sum(T1DB$Chla < 1.2, na.rm=TRUE)
sum(T1PW$Chla < 1.2, na.rm=TRUE)

sum(T2CI20$Chla < 1.2, na.rm=TRUE)
sum(T2CI5$Chla < 1.2, na.rm=TRUE)
sum(T2DB$Chla < 1.2, na.rm=TRUE)
sum(T2PW$Chla < 1.2, na.rm=TRUE)


#DO
sum(T1CI20$DO >9.4, na.rm=TRUE)
sum(T1CI5$DO >9.4, na.rm=TRUE)
sum(T1DB$DO >9.4, na.rm=TRUE)
sum(T1PW$DO >9.4, na.rm=TRUE)

sum(T2CI20$DO >9.4, na.rm=TRUE)
sum(T2CI5$DO >9.4, na.rm=TRUE)
sum(T2DB$DO >9.4, na.rm=TRUE)
sum(T2PW$DO >9.4, na.rm=TRUE)

sum(T1CI20$DO < 7, na.rm=TRUE)
sum(T1CI5$DO < 7, na.rm=TRUE)
sum(T1DB$DO < 7, na.rm=TRUE)
sum(T1PW$DO < 7, na.rm=TRUE)

sum(T2CI20$DO < 7, na.rm=TRUE)
sum(T2CI5$DO < 7, na.rm=TRUE)
sum(T2DB$DO < 7, na.rm=TRUE)
sum(T2PW$DO < 7, na.rm=TRUE)



#pH
sum(T1CI20$pH >8.05, na.rm=TRUE)
sum(T1CI5$pH >8.05, na.rm=TRUE)
sum(T1DB$pH >8.05, na.rm=TRUE)
sum(T1PW$pH >8.05, na.rm=TRUE)

sum(T2CI20$pH >8.05, na.rm=TRUE)
sum(T2CI5$pH >8.05, na.rm=TRUE)
sum(T2DB$pH >8.05, na.rm=TRUE)
sum(T2PW$pH >8.05, na.rm=TRUE)

sum(T1CI20$pH < 7.79, na.rm=TRUE)
sum(T1CI5$pH < 7.79, na.rm=TRUE)
sum(T1DB$pH < 7.79, na.rm=TRUE)
sum(T1PW$pH < 7.79, na.rm=TRUE)

sum(T2CI20$pH < 7.79, na.rm=TRUE)
sum(T2CI5$pH < 7.79, na.rm=TRUE)
sum(T2DB$pH < 7.79, na.rm=TRUE)
sum(T2PW$pH < 7.79, na.rm=TRUE)



#ara
sum(T1CI20$ara >1.9, na.rm=TRUE)
sum(T1CI5$ara >1.9, na.rm=TRUE)
sum(T1DB$ara >1.9, na.rm=TRUE)
sum(T1PW$ara >1.9, na.rm=TRUE)

sum(T2CI20$ara >1.9, na.rm=TRUE)
sum(T2CI5$ara >1.9, na.rm=TRUE)
sum(T2DB$ara >1.9, na.rm=TRUE)
sum(T2PW$ara >1.9, na.rm=TRUE)

sum(T1CI20$ara < 1.1, na.rm=TRUE)
sum(T1CI5$ara < 1.1, na.rm=TRUE)
sum(T1DB$ara < 1.1, na.rm=TRUE)
sum(T1PW$ara < 1.1, na.rm=TRUE)

sum(T2CI20$ara < 1.1, na.rm=TRUE)
sum(T2CI5$ara < 1.1, na.rm=TRUE)
sum(T2DB$ara < 1.1, na.rm=TRUE)
sum(T2PW$ara < 1.1, na.rm=TRUE)