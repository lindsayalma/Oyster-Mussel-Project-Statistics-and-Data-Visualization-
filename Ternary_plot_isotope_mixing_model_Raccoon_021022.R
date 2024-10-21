library(readxl)
d <- read_excel("C:/Users/lalma/Dropbox/Raccoon/Isotopes/Isotope_mixing_model_Raccoon_021022.xlsx")

shape<-d$shape
site<-d$site
#load the libraries for the ggplot2 and ggtern packages
#(this assumes you've already obtained the packages from CRAN - see version and install notes at the bottom of this document)
library(ggplot2)
library(ggtern)
q<-ggtern(data=d, aes(x=Terrestrial, y=Phytobenthos, z=Phytoplankton)) +   
  labs(color = "Cohort")+theme_custom(
    base_size = 12,
    base_family = "",
    tern.plot.background = NULL,
    tern.panel.background = "white",
    col.T = "darkgreen",
    col.L = "black",
    col.R = "blue",
    col.grid.minor = "white"
  )
q
q+theme_showarrows()+geom_point(aes(color=site,shape=as.factor(shape)),size=2)+theme(legend.key=element_blank())+scale_color_manual(values=c("#7c7c7c", "#04c390", "#0451a4","#ec844c","#8c64d4", "#04c390", "#0451a4","#ec844c","#8c64d4"))



p<- ggtern(data=d, aes(x=Terrestrial, y=Phytobenthos, z=Phytoplankton)) +   
    labs(color = "Cohort")+theme_custom(
    base_size = 12,
    base_family = "",
    tern.plot.background = NULL,
    tern.panel.background = "white",
    col.T = "darkgreen",
    col.L = "black",
    col.R = "blue",
    col.grid.minor = "white"
  )


p+ theme_showarrows()+geom_point(aes(colour = factor(Lake)),size=3,shape=as.factor(shape))+theme(legend.key=element_blank())+scale_color_manual(values=c("#7c7c7c", "#04c390", "#0451a4","#ec844c","#8c64d4", "#04c390", "#0451a4","#ec844c","#8c64d4"))



