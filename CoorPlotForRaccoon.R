library(Hmisc)
library(readxl)
library(corrplot)
        
##CI20
Oceanographic_Correlation_Matrix_CI20 <- read_excel("Oceanographic_Correlation_Matrix_CI20.xlsx")
View(Oceanographic_Correlation_Matrix_CI20)
        

M<-cor(Oceanographic_Correlation_Matrix_CI20)
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
p.mat <- cor.mtest(Oceanographic_Correlation_Matrix_CI20)
head(p.mat[, 1:5])
corrplot(M, type="upper", order="alphabet", 
         p.mat = p.mat, sig.level = 0.05, insig = "blank")

View(p.mat)
p.mat
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="alphabet", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

#find p and r values 
res2 <- rcorr(as.matrix(M))
res2
res2$r
res2$P

res1 <- cor.mtest(M, conf.level = .95)
res1

## specialized the insignificant value according to the significant level
corrplot(M, p.mat = res1$p, sig.level = .05)


##double plot with r values and p<0.05 crossed out
corrplot.mixed(M, upper = "square", lower.col = "black", order="alphabet", number.cex = .8, tl.col = "black", p.mat=p.mat)

col <- colorRampPalette(c("green", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot.mixed(M, method="color", col=col(200),  
         type="upper", order="alphabet", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)




library("PerformanceAnalytics")
chart.Correlation(M, histogram=TRUE, pch=19)



##CI5
Oceanographic_Correlation_Matrix_CI5 <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/Oceanographic_Correlation_Matrix_CI5.xlsx")
library(corrplot)
M<-cor(Oceanographic_Correlation_Matrix_CI5)
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
p.mat <- cor.mtest(Oceanographic_Correlation_Matrix_CI5)
head(p.mat[, 1:5])

corrplot(M, type="upper", order="alphabet", 
         p.mat = p.mat, sig.level = 0.05, insig = "blank")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="alphabet", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

#find p and r values 
res2 <- rcorr(as.matrix(M))
res2
res2$r
res2$P

res1 <- cor.mtest(M, conf.level = .95)


## specialized the insignificant value according to the significant level
corrplot(M, p.mat = res1$p, sig.level = .05)


##double plot with r values and p<0.05 crossed out
corrplot.mixed(M, upper = "square", lower.col = "black", order="alphabet", number.cex = .8, tl.col = "black", p.mat=p.mat)




##DB
Oceanographic_Correlation_Matrix_DB <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/Oceanographic_Correlation_Matrix_DB.xlsx")
View(Oceanographic_Correlation_Matrix_DB)

library(corrplot)
M<-cor(Oceanographic_Correlation_Matrix_DB)
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
p.mat <- cor.mtest(Oceanographic_Correlation_Matrix_DB)
head(p.mat[, 1:5])

corrplot(M, type="upper", order="alphabet", 
         p.mat = p.mat, sig.level = 0.05, insig = "blank")


View(p.mat)
View(M)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="alphabet", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

#find p and r values 
res2 <- rcorr(as.matrix(M))
res2
res2$r
res2$P

res1 <- cor.mtest(M, conf.level = .95)
res1

## specialized the insignificant value according to the significant level
corrplot(M, p.mat = res1$p, sig.level = .05)


##double plot with r values and p<0.05 crossed out
corrplot.mixed(M, upper = "square", lower.col = "black", order="alphabet", number.cex = .8, tl.col = "black", p.mat=p.mat)




##PW
Oceanographic_Correlation_Matrix_PW <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Ocean/Oceanographic_Correlation_Matrix_PW.xlsx")
View(Oceanographic_Correlation_Matrix_PW)


library(corrplot)
M<-cor(Oceanographic_Correlation_Matrix_PW)
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
p.mat <- cor.mtest(Oceanographic_Correlation_Matrix_PW)
head(p.mat[, 1:5])

corrplot(M, type="upper", order="alphabet", 
         p.mat = p.mat, sig.level = 0.05, insig = "blank")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="alphabet", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05,
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

