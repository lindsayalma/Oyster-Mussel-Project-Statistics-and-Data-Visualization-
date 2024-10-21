library(readxl)
setwd("C:/Users/lalma/Dropbox/Raccoon/Isotopes")


##FIRST, ASSIGN TROPHIC ENRICHMENT FACTORS#### (To Lindsay, these are the assignments of trophic enrichment factors, I tested several senarios)
set.seed(9) #use 9, Mia Hamm (To lindsay, I set a seed so that I can see if my results are the same as I check)

## LINDSAY READ THIS:
## mu always means "mean"
## sigma always means "standard diviation"
## TEF means "trophic enrichment factor".
## C is carbon, N is nitrogen
## fL, fP, fT means "fraction Littoral", "fraction Pelagic", "fraction Terrestrial" These are the three fractions that sum to 1. The portions.


## Assign trophic enrichment values.
TEF_C.mu <- 2.9 #conway
TEF_N.mu <- 4.7 #conway
TEF_C.sigma <- 1.1 #conway
TEF_N.sigma <- 1.1  #conway

# Second, restart the settings for the Monte Carlo Simulation####

ndraws <- 10000
maxlimit <- 1 #This is the % that I'm willing to have or not have to remove crazy solutions
minlimit <- 0
count <- 1 ##This manages my output-matrix filling system, not sure how to fix this
# since I don't have an index to count all the fish, just i and j for lakes/
# 10/3/2019 - I just spoke with Julian, he mentioned that sometimes people use a code 
# that means "append" and that just means, "add this to the bottom of my table!" I will 
# Look that up, because if I forget to reset the count, it will start somewhere in the middle.


#########################################oyster#########################

#read in the file
fish4MonteCarlo <-read_excel("CN_site_isotope_mixing_model_data_imput_Racoon_020722.xlsx", 
           sheet = "oyster_mean_sd")
fish4MonteCarlo <- read_excel("C:/Users/lalma/Dropbox/Raccoon/Isotopes/CN_site_isotope_mixing_model_data_imput_Racoon_020722.xlsx")  ##this is a file with my fish, C, N data, with columns identifying which lake they were from
#lit_CN <- read.csv(file = "littoral.csv") ##this is a file with my littoral endmembers, C, N data, with columns identifying which lake they were from
#pelag_CN <- read.csv(file = "pelagic.csv") ##this is a file with my pelag endmembers, C, N data, with columns identifying which lake they were from
#ter_CN <- read.csv(file = "ter.csv") ##this is a file with my terrestrial endmembers, C, N data, with columns identifying which lake they were from
#Now I realize I could have had them all together in one file with a column to id what they were. AND I did this before I knew how to use the tidyverse.

##THIRD CREATE OUTPUT MATRIX ####
MonteCarloOutput <- matrix(NA, nrow = nrow(fish4MonteCarlo), ncol = 9)
colnames(MonteCarloOutput) <-c("Lake", "Identifier.1", "fT.mu", "fT.sigma", "fP.mu", "fP.sigma", "fL.mu", "fL.sigma", "count")

# Get a vector of lake names so that I can loop through all the lakes.
  Lakenames <-as.vector(unique(fish4MonteCarlo$SiteDepthDate))
Lakenames<-Lakenames[-(10:10)]#remove the NA at the end

fish4MonteCarlo<-fish4MonteCarlo[-c(115:124),]
View(fish4MonteCarlo)

for (j in 1:length(Lakenames)){
  #first, I need the fish data from the lake I'm working on (lake "j")
  lakefish <-fish4MonteCarlo[(which(fish4MonteCarlo$Site ==  Lakenames[j])),
                     c("C", "N")]
  
  
  #Then get the data for the three sources from that lake (j) 
  #Littoral sources - so the results from the rocks
#phyto
  Lit.d13C.mu <- -22.8
  Lit.d15N.mu <- 6.57
  Lit.d13C.sigma <- 3.55
  Lit.d15N.sigma <- 0.22
  
  #Pelagic - from the pelagic area
#macrobenthosalgae
  Pelag.d13C.mu <- -13.61
  Pelag.d15N.mu <- 7.03
  Pelag.d13C.sigma <- 3.57
  Pelag.d15N.sigma <- 2.32
  
  #Terrestrial - from the surrounding plants
 
  Ter.d13C.mu <- -30.99
  Ter.d15N.mu <- -0.35
  Ter.d13C.sigma <- 0.94
  Ter.d15N.sigma <- 1.68
  
  #then, after gathring all the data, I start looking at the fish from the lake
  for(i in 1:nrow(lakefish)){
    #create the temp matrix that is going to store all the draws, probably 10000
    temp.montecarlodraws <- matrix(NA, nrow = ndraws, ncol = 3)
    
    for(k in 1:ndraws){
      #Identify my fish starting values
      d13C <- as.numeric(lakefish[i,1]) - rnorm(1,mean = TEF_C.mu, sd = TEF_C.sigma)
      d15N <- as.numeric(lakefish[i,2]) - rnorm(1, mean = TEF_N.mu, sd = TEF_N.sigma)
      
      # I randomly draw my C source values based on the lake mean and sd for d13C
      Lit.d13C <- rnorm(1, mean = Lit.d13C.mu, sd = Lit.d13C.sigma)
      Pelag.d13C <- rnorm(1, mean = Pelag.d13C.mu, sd = Pelag.d13C.sigma)
      Ter.d13C <- rnorm(1, mean = Ter.d13C.mu, sd = Ter.d13C.sigma)
      
      #Same as above, but for d15N no N TEF.
      Lit.d15N <- rnorm(1, mean = Lit.d15N.mu, sd = Lit.d15N.sigma)
      Pelag.d15N <- rnorm(1, mean = Pelag.d15N.mu, sd = Pelag.d15N.sigma)
      Ter.d15N <- rnorm(1, mean = Ter.d15N.mu, sd = Ter.d15N.sigma)
      
      ##Now I put this data together into a basic matrix, which is basically
      ## just the same as algebra with three equations and three variables, but instead
      ## of keeping my uknown variables, I just do the steps one by one within a matrix
      row1 <- c(Lit.d13C, Pelag.d13C, Ter.d13C, d13C)
      row2 <- c(Lit.d15N, Pelag.d15N, Ter.d15N, d15N)
      row3 <- c(1,1,1,1)
      
      #make the matrix
      iso.m <- matrix(c(row1, row2, row3),
                      nrow = 3, ncol = 4, byrow = TRUE, 
                      dimnames = list(c("C", "N", "sum"),c("Lit", "Pelag", "Ter", "mix")))
      #find the number that will drop the first term of the second equation
      first.multiplyer <- -1*(iso.m[2,1]/iso.m[1,1])
      #multipy the first row by the multiplyer
      iso.row2 <- iso.m[1,]*first.multiplyer
      # add the values to the second row, this should result in the first term of 
      # the second row being zero. If it is not, there is a problem
      row2.new <-iso.m[2,]+iso.row2
      
      # find the number that will drop the first term of the third row
      second.multiplyer <- -1*(iso.m[3,1]/iso.m[1,1])
      # multiply the first row by the multipler
      iso.row3 <- iso.m[1,]*second.multiplyer
      # add the third row and first row, now the third row should start with a zero.
      row3.new <- iso.m[3,]+iso.row3
      
      #Re build my new matrix, and I now have two unknown terms and two equations 
      # if I only look at the bottom two rows.
      iso.m2 <- matrix(c(row1, row2.new, row3.new),
                       nrow = 3, ncol = 4, byrow = TRUE, 
                       dimnames = list(c("C", "N", "sum"),c("Lit", "Pelag", "Ter", "mix")))
      # Working with just the second and third rows I repeat the steps above
      third.multiplyer <- -1*(iso.m2[3,2]/iso.m2[2,2])
      iso.row3.2 <- iso.m2[2,]*third.multiplyer
      row3.new.new <- iso.m2[3,]+iso.row3.2
      # Now I have a beauitful matrix with the last row having one missing term!
      iso.m3 <- matrix(c(row1, row2.new, row3.new.new),
                       nrow = 3, ncol = 4, byrow = TRUE, 
                       dimnames = list(c("C", "N", "sum"),c("Lit", "Pelag", "Ter", "mix")))
      # I can solve for the first term
      fT <- iso.m3[3,4]/iso.m3[3,3]
      # I use the second term in the second row to solve for the second term.
      fP <- (iso.m3[2,4]-(iso.m3[2,3]*fT))/iso.m3[2,2]
      # Now I can solve for the third term by plugging in my two new values in the first
      # equation.
      fL <- (iso.m3[1,4] -(iso.m3[1,3]*fT) - (iso.m3[1,2]*fP))/iso.m3[1,1]
 
      #The script that I built this algebra in has a self-check where I sum the values
      # and I use the the solutions in the original equations to make sure they work
      
      #Put the three values into a temp matrix.
      temp.montecarlodraws[k,1]<- fT
      temp.montecarlodraws[k,2]<- fP
      temp.montecarlodraws[k,3]<- fL
      
      ### By doing this one by one, I think there are some rare situations where the unique solution
      ### Is huge!! For example for TOB_RBT_07 had a solution that was 86, 83, and -169. 
      ### I need to constrain these wierd ones.What if I dropped them from the temp storage?
      
    }
    
    temp.montecarlodraws <- temp.montecarlodraws[(which(!temp.montecarlodraws[,1] > maxlimit)),]
    temp.montecarlodraws <- temp.montecarlodraws[(which(!temp.montecarlodraws[,1] < minlimit)),]
    temp.montecarlodraws <- temp.montecarlodraws[(which(!temp.montecarlodraws[,2] > maxlimit)),]
    temp.montecarlodraws <- temp.montecarlodraws[(which(!temp.montecarlodraws[,2] < minlimit)),]
    temp.montecarlodraws <- temp.montecarlodraws[(which(!temp.montecarlodraws[,3] > maxlimit)),]
    temp.montecarlodraws <- temp.montecarlodraws[(which(!temp.montecarlodraws[,3] < minlimit)),]
   
     #Now my matrix is three columns by 10000 rows. I get the mean and sd of each!
    MonteCarloOutput[count,1]<- Lakenames[j] #lake id
    #MonteCarloOutput[count,2]<- as.character(lakefish[i,3]) #Fish ID
    MonteCarloOutput[count,3]<- mean(temp.montecarlodraws[,1]) 
    MonteCarloOutput[count,4]<- sd(temp.montecarlodraws[,1])
    MonteCarloOutput[count,5]<- mean(temp.montecarlodraws[,2])
    MonteCarloOutput[count,6]<- sd(temp.montecarlodraws[,2])
    MonteCarloOutput[count,7]<- mean(temp.montecarlodraws[,3])
    MonteCarloOutput[count,8]<- sd(temp.montecarlodraws[,3])
    MonteCarloOutput[count,9]<- length(temp.montecarlodraws[,3])
    count <- count+1
  }
  
}

MonteCarloOutput <- as.data.frame(MonteCarloOutput)
MonteCarloOutput

#Last, save this an an output data file
write.csv(MonteCarloOutput, file = "MonteCarloOutput.csv")


#MonteCarloOutputfilename <- paste0("output/data/MonteCarloOutput", scenario2read ,".csv")
#write.csv(x = MonteCarloOutput, file = MonteCarloOutputfilename)




#########################################mussel#########################
#read in the file
fish4MonteCarlo <-read_excel("CN_site_isotope_mixing_model_data_imput_Racoon_020722.xlsx", 
                             sheet = "mussel_mean_sd")
fish4MonteCarlo   ##this is a file with my fish, C, N data, with columns identifying which lake they were from
#lit_CN <- read.csv(file = "littoral.csv") ##this is a file with my littoral endmembers, C, N data, with columns identifying which lake they were from
#pelag_CN <- read.csv(file = "pelagic.csv") ##this is a file with my pelag endmembers, C, N data, with columns identifying which lake they were from
#ter_CN <- read.csv(file = "ter.csv") ##this is a file with my terrestrial endmembers, C, N data, with columns identifying which lake they were from
#Now I realize I could have had them all together in one file with a column to id what they were. AND I did this before I knew how to use the tidyverse.

##THIRD CREATE OUTPUT MATRIX ####
MonteCarloOutput <- matrix(NA, nrow = nrow(fish4MonteCarlo), ncol = 9)
colnames(MonteCarloOutput) <-c("Lake", "Identifier.1", "fT.mu", "fT.sigma", "fP.mu", "fP.sigma", "fL.mu", "fL.sigma", "count")

# Get a vector of lake names so that I can loop through all the lakes.
Lakenames <-as.vector(unique(fish4MonteCarlo$Site))
Lakenames<-Lakenames[-(10:10)]#remove the NA at the end

fish4MonteCarlo<-fish4MonteCarlo[-c(115:124),]
View(fish4MonteCarlo)

for (j in 1:length(Lakenames)){
  #first, I need the fish data from the lake I'm working on (lake "j")
  lakefish <-fish4MonteCarlo[(which(fish4MonteCarlo$Site ==  Lakenames[j])),
                             c("C", "N")]
  
  
  #Then get the data for the three sources from that lake (j) 
  #Littoral sources - so the results from the rocks
  #phyto
  Lit.d13C.mu <- -22.8
  Lit.d15N.mu <- 6.57
  Lit.d13C.sigma <- 3.55
  Lit.d15N.sigma <- 0.22
  
  #Pelagic - from the pelagic area
  #macrobenthosalgae
  Pelag.d13C.mu <- -13.61
  Pelag.d15N.mu <- 7.03
  Pelag.d13C.sigma <- 3.57
  Pelag.d15N.sigma <- 2.32
  
  #Terrestrial - from the surrounding plants
  
  Ter.d13C.mu <- -30.99
  Ter.d15N.mu <- -0.35
  Ter.d13C.sigma <- 0.94
  Ter.d15N.sigma <- 1.68
  
  #then, after gathring all the data, I start looking at the fish from the lake
 for(i in 1:nrow(lakefish)){
    #create the temp matrix that is going to store all the draws, probably 10000
    temp.montecarlodraws <- matrix(NA, nrow = ndraws, ncol = 3)
    
    for(k in 1:ndraws){
      #Identify my fish starting values
      d13C <- as.numeric(lakefish[i,1]) - rnorm(1,mean = TEF_C.mu, sd = TEF_C.sigma)
      d15N <- as.numeric(lakefish[i,2]) - rnorm(1, mean = TEF_N.mu, sd = TEF_N.sigma)
      
      # I randomly draw my C source values based on the lake mean and sd for d13C
      Lit.d13C <- rnorm(1, mean = Lit.d13C.mu, sd = Lit.d13C.sigma)
      Pelag.d13C <- rnorm(1, mean = Pelag.d13C.mu, sd = Pelag.d13C.sigma)
      Ter.d13C <- rnorm(1, mean = Ter.d13C.mu, sd = Ter.d13C.sigma)
      
      #Same as above, but for d15N no N TEF.
      Lit.d15N <- rnorm(1, mean = Lit.d15N.mu, sd = Lit.d15N.sigma)
      Pelag.d15N <- rnorm(1, mean = Pelag.d15N.mu, sd = Pelag.d15N.sigma)
      Ter.d15N <- rnorm(1, mean = Ter.d15N.mu, sd = Ter.d15N.sigma)
      
      ##Now I put this data together into a basic matrix, which is basically
      ## just the same as algebra with three equations and three variables, but instead
      ## of keeping my uknown variables, I just do the steps one by one within a matrix
      row1 <- c(Lit.d13C, Pelag.d13C, Ter.d13C, d13C)
      row2 <- c(Lit.d15N, Pelag.d15N, Ter.d15N, d15N)
      row3 <- c(1,1,1,1)
      
      #make the matrix
      iso.m <- matrix(c(row1, row2, row3),
                      nrow = 3, ncol = 4, byrow = TRUE, 
                      dimnames = list(c("C", "N", "sum"),c("Lit", "Pelag", "Ter", "mix")))
      #find the number that will drop the first term of the second equation
      first.multiplyer <- -1*(iso.m[2,1]/iso.m[1,1])
      #multipy the first row by the multiplyer
      iso.row2 <- iso.m[1,]*first.multiplyer
      # add the values to the second row, this should result in the first term of 
      # the second row being zero. If it is not, there is a problem
      row2.new <-iso.m[2,]+iso.row2
      
      # find the number that will drop the first term of the third row
      second.multiplyer <- -1*(iso.m[3,1]/iso.m[1,1])
      # multiply the first row by the multipler
      iso.row3 <- iso.m[1,]*second.multiplyer
      # add the third row and first row, now the third row should start with a zero.
      row3.new <- iso.m[3,]+iso.row3
      
      #Re build my new matrix, and I now have two unknown terms and two equations 
      # if I only look at the bottom two rows.
      iso.m2 <- matrix(c(row1, row2.new, row3.new),
                       nrow = 3, ncol = 4, byrow = TRUE, 
                       dimnames = list(c("C", "N", "sum"),c("Lit", "Pelag", "Ter", "mix")))
      # Working with just the second and third rows I repeat the steps above
      third.multiplyer <- -1*(iso.m2[3,2]/iso.m2[2,2])
      iso.row3.2 <- iso.m2[2,]*third.multiplyer
      row3.new.new <- iso.m2[3,]+iso.row3.2
      # Now I have a beauitful matrix with the last row having one missing term!
      iso.m3 <- matrix(c(row1, row2.new, row3.new.new),
                       nrow = 3, ncol = 4, byrow = TRUE, 
                       dimnames = list(c("C", "N", "sum"),c("Lit", "Pelag", "Ter", "mix")))
      # I can solve for the first term
      fT <- iso.m3[3,4]/iso.m3[3,3]
      # I use the second term in the second row to solve for the second term.
      fP <- (iso.m3[2,4]-(iso.m3[2,3]*fT))/iso.m3[2,2]
      # Now I can solve for the third term by plugging in my two new values in the first
      # equation.
      fL <- (iso.m3[1,4] -(iso.m3[1,3]*fT) - (iso.m3[1,2]*fP))/iso.m3[1,1]
      
      #The script that I built this algebra in has a self-check where I sum the values
      # and I use the the solutions in the original equations to make sure they work
      
      #Put the three values into a temp matrix.
      temp.montecarlodraws[k,1]<- fT
      temp.montecarlodraws[k,2]<- fP
      temp.montecarlodraws[k,3]<- fL
      
      ### By doing this one by one, I think there are some rare situations where the unique solution
      ### Is huge!! For example for TOB_RBT_07 had a solution that was 86, 83, and -169. 
      ### I need to constrain these wierd ones.What if I dropped them from the temp storage?
      
    }
    
    temp.montecarlodraws <- temp.montecarlodraws[(which(!temp.montecarlodraws[,1] > maxlimit)),]
    temp.montecarlodraws <- temp.montecarlodraws[(which(!temp.montecarlodraws[,1] < minlimit)),]
    temp.montecarlodraws <- temp.montecarlodraws[(which(!temp.montecarlodraws[,2] > maxlimit)),]
    temp.montecarlodraws <- temp.montecarlodraws[(which(!temp.montecarlodraws[,2] < minlimit)),]
    temp.montecarlodraws <- temp.montecarlodraws[(which(!temp.montecarlodraws[,3] > maxlimit)),]
    temp.montecarlodraws <- temp.montecarlodraws[(which(!temp.montecarlodraws[,3] < minlimit)),]
    
    #Now my matrix is three columns by 10000 rows. I get the mean and sd of each!
    MonteCarloOutput[count,1]<- Lakenames[j] #lake id
    #MonteCarloOutput[count,2]<- as.character(lakefish[i,3]) #Fish ID
    MonteCarloOutput[count,3]<- mean(temp.montecarlodraws[,1]) 
    MonteCarloOutput[count,4]<- sd(temp.montecarlodraws[,1])
    MonteCarloOutput[count,5]<- mean(temp.montecarlodraws[,2])
    MonteCarloOutput[count,6]<- sd(temp.montecarlodraws[,2])
    MonteCarloOutput[count,7]<- mean(temp.montecarlodraws[,3])
    MonteCarloOutput[count,8]<- sd(temp.montecarlodraws[,3])
    MonteCarloOutput[count,9]<- length(temp.montecarlodraws[,3])
    count <- count+1
  }
  
}

MonteCarloOutput <- as.data.frame(MonteCarloOutput)
MonteCarloOutput

#Last, save this an an output data file
write.csv(MonteCarloOutput, file = "MonteCarloOutput_mus.csv")


#MonteCarloOutputfilename <- paste0("output/data/MonteCarloOutput", scenario2read ,".csv")
#write.csv(x = MonteCarloOutput, file = MonteCarloOutputfilename)



