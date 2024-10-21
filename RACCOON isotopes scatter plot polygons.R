library(readxl)
library(agricolae)

FIXEDRaccoonIsotopesForR <- read_excel("C:/Users/lalma/Dropbox/Raccoon/Isotopes/FIXEDRaccoonIsotopesForR.xlsx")


library(ggplot2)
library(plyr)
work <- "C:/Users/Lindsay/Dropbox/Raccoon/Isotopes/"
setwd(work)

M<-subset(FIXEDRaccoonIsotopesForR, Species=="M")
O<-subset(FIXEDRaccoonIsotopesForR, Species=="O")
View(M)

View(O)

#note you have some missing data
nomissing <- na.omit(M) #chull function does not work with missing data
df$`d15N vs Air N2 (permil)`
df$`d13C vs VPDB (permil)`

##MUSSEL
#getting the convex hull of each unique point set
df <- M
View(df)
find_hull <- function(df) df[chull(df$C,df$N), ]
hulls <- ddply(df, "Title", find_hull)
View(hulls)

plot <- ggplot(data = df, aes(x = `C`, y = `N`, colour=Title, fill = Title)) +
  geom_point() +  
  scale_color_manual(values = c('gray33', 'darkgreen', 'blue4', 'darkred', 'darkgoldenrod', '#33CC66','steelblue','red','darkgoldenrod1')) + 
  scale_fill_manual(values = c('gray33', 'darkgreen', 'blue4', 'darkred', 'darkgoldenrod', '#33CC66','steelblue','red','darkgoldenrod1')) + 
  geom_polygon(data = hulls, alpha = 0.5) +  xlim(-24,-16) + ylim(8,11.5)+
  labs(x = "d13C", y = "d15N")
plot+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


##OYSTER
#getting the convex hull of each unique point set
df <- O
View(df)
find_hull <- function(df) df[chull(df$C,df$N), ]
hulls <- ddply(df, "Title", find_hull)
View(hulls)

plot <- ggplot(data = df, aes(x = `C`, y = `N`, colour=Title, fill = Title)) +
  geom_point() + 
  scale_color_manual(values = c('gray33', 'darkgreen', 'blue4', 'darkred', 'darkgoldenrod', '#33CC66','steelblue','red','darkgoldenrod1')) + 
  scale_fill_manual(values = c('gray33', 'darkgreen', 'blue4', 'darkred', 'darkgoldenrod', '#33CC66','steelblue','red','darkgoldenrod1')) + 
  geom_polygon(data = hulls, alpha = 0.5) +  xlim(-24,-16) + ylim(8,11.5)+
  labs(x = "d13C", y = "d15N")
plot+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))





M$N

aggregate(N~ Date+SiteDepth, data=M, mean)
aggregate(N~ Date+SiteDepth, data=M, sd)
aggregate(N~ Date+SiteDepth, data=M, length)

aggregate(N~ Date+SiteDepth, data=O, mean)
aggregate(N~ Date+SiteDepth, data=O, sd)
aggregate(N~ Date+SiteDepth, data=O, length)

aggregate(C~ Date+SiteDepth, data=M, mean)
aggregate(C~ Date+SiteDepth, data=M, sd)
aggregate(C~ Date+SiteDepth, data=M, length)

aggregate(C~ Date+SiteDepth, data=O, mean)
aggregate(C~ Date+SiteDepth, data=O, sd)
aggregate(C~ Date+SiteDepth, data=O, length)

aggregate(`C:N`~ Date+SiteDepth, data=O, mean)
aggregate(`C:N`~ Date+SiteDepth, data=O, sd)
aggregate(`C:N`~ Date+SiteDepth, data=O, length)

aggregate(`C:N`~ Date+SiteDepth, data=M, mean)
aggregate(`C:N`~ Date+SiteDepth, data=M, sd)
aggregate(`C:N`~ Date+SiteDepth, data=M, length)






aov<-aov(N~Date * SiteDepth, data=M)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "SiteDepth"), group=TRUE)
p
plot(p, las=2)


aov<-aov(N~Date * SiteDepth, data=O)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "SiteDepth"), group=TRUE)
p
plot(p, las=2)

aov<-aov(`C:N`~Date * SiteDepth, data=M)
summary(aov)
TukeyHSD(aov)
p<-HSD.test(aov, c( "Date", "SiteDepth"), group=TRUE)
p
plot(p, las=2)