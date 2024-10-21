library(readxl)
SDgraphData <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Isotopes/SDgraphData.xlsx")
SDgraphData_oysteronly <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/Isotopes/SDgraphData_oysteronly.xlsx")
View(SDgraphData)
library(ggthemes)

ggplot(data = SDgraphData,aes(x = C,y = N, color=Group)) +  
  labs(x = "d13C", y="d15N")+
  geom_point(aes(shape=Type, color=Group))+
  geom_errorbar(aes(ymin = N-Nsd1,ymax = N+Nsd2)) + 
  geom_errorbarh(aes(xmin = C-Csd1,xmax = C+Csd2))+
  theme_classic()+
  scale_color_manual(values=c("black","darkgreen", "blue", "red", "darkgoldenrod", "green","skyblue", "coral1", "gold"))

#oysteronly
ggplot(data = SDgraphData_oysteronly,aes(x = C,y = N, color=Group)) +  
  labs(x = "d13C", y="d15N")+
  geom_point(aes(color=Group))+
  geom_errorbar(aes(ymin = N-Nsd1,ymax = N+Nsd2)) + 
  geom_errorbarh(aes(xmin = C-Csd1,xmax = C+Csd2))+
  theme_classic()+
  scale_color_manual(values=c("darkgreen", "blue", "red", "darkgoldenrod", "green","skyblue", "coral1", "gold"))


