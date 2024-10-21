
library(readxl)
SSG <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Correlations/SSG.xlsx")
View(SSG)

library(readxl)
SScorrGrowth <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Correlations/SScorrGrowth.xlsx", 
                           sheet = "T2")
View(SScorrGrowth)

library(readxl)
T1T2SS_ave <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Shell Strength/T1T2SS_ave.xlsx")
View(T1T2SS_ave)

T2<-merge(SScorrGrowth,T1T2SS_ave,by="ID")
View(T2)
write.csv(T2, file = "C:/Users/Lindsay/Dropbox/Raccoon/Correlations/T2.csv", row.names = T)

aggregate(Force~Sitedepth+Date+ Species, SScorrGrowth, length)


library(corrplot)

M<-SSG

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

library(readxl)
RaccoonLipids <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Lipids and Fatty Acids/RaccoonLipids.xlsx")
View(RaccoonLipids)
library(readxl)
FIXEDRaccoonIsotopesForR <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Isotopes/FIXEDRaccoonIsotopesForR.xlsx")
View(FIXEDRaccoonIsotopesForR)


LipidsCorrIsotopes<-merge(FIXEDRaccoonIsotopesForR,RaccoonLipids,by="ID")
View(LipidsCorrIsotopes)
write.csv(LipidsCorrIsotopes, file = "C:/Users/Lindsay/Dropbox/Raccoon/Correlations/LipidsCorrIsotopes.csv", row.names = T)

aggregate(`%lipid`~SiteDepth.x+Date.x+ Species.x, LipidsCorrIsotopes, length)

library(readxl)
GrowthForCorr <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Correlations/GrowthForCorr.xlsx")
View(GrowthForCorr)


LipidsCorrGrowth<-C
merge(GrowthForCorr,RaccoonLipids,by="ID")
View(LipidsCorrGrowth)
write.csv(LipidsCorrGrowth, file = "C:/Users/Lindsay/Dropbox/Raccoon/Correlations/LipidsCorrGrowth.csv", row.names = T)

aggregate(`%lipid`~Sitedepth+Date.x+ Species, LipidsCorrGrowth, length)

IsotopesCorrGrowth<-merge(GrowthForCorr,FIXEDRaccoonIsotopesForR,by="ID")
View(IsotopesCorrGrowth)
write.csv(IsotopesCorrGrowth, file = "C:/Users/Lindsay/Dropbox/Raccoon/Correlations/IsotopesCorrGrowth.csv", row.names = T)

aggregate(C~Sitedepth+Date.x+ Species, IsotopesCorrGrowth, length)


library(readxl)
T1T2SS_ave <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Shell Strength/T1T2SS_ave.xlsx")
View(T1T2SS_ave)

library(readxl)
RaccoonLipids <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Lipids and Fatty Acids/RaccoonLipids.xlsx")
View(RaccoonLipids)

LipidsCorrSS<-merge(T1T2SS_ave,RaccoonLipids,by="ID")
View(LipidsCorrSS)
write.csv(LipidsCorrSS, file = "C:/Users/Lindsay/Dropbox/Raccoon/Correlations/LipidsCorrSS.csv", row.names = T)

aggregate(`%lipid`~SiteDepth+Date.x+ Species.x, LipidsCorrSS, length)

