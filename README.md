# MAQ11306
#MAQ11306 - Project week
#### Black carbon measurements
#BC units = ng/m^3
#Location: BG= background (clean air) F=Forum entrance (smoking)
 
setwd("~/R/DATA")

#LIBRARY 
library(readr)
library(reshape)
library(ggmap)
library(ggplot2)
library(oce)
library(effects)
library(stats)
library(stats4)
library(vegan)
library(car)

#Import dataset

#Black carbon 
BC1 <- MAG11306_projectBC1

BC2 <- MAG11306_projectBC2

#Subset of data, divided by location
BG1 <- subset( BC1, BC1$Location=="BG")
BG2 <- subset( BC2, BC2$Location=="BG")
F1 <- subset( BC1, BC1$Location=="F")
F2 <- subset( BC2, BC2$Location=="F")

#Particles PC_07
PC <- MAG11306_projectPC07
PC_BG <- subset(PC, Location=="BG")
PC_F <- subset(PC, Location=="F")


#DATA ANALYSIS + GRAPHS 
#mean and standard deviation of each location 

#Black carbon
#BC device 1
mean(BG1$BC) #[1] 131.8485 background
mean(F1$BC) #[1] 313.2123 Forum

sd(BG1$BC) #1] 22102.1 background
sd(F1$BC) #[1] 94567.37

#BC device 2
mean(BG2$BC) #[1] -461.6515 background
mean(F2$BC) #[1] 105.9707 Forum

sd(BG2$BC) #[1] 199412 background
sd(F2$BC) #[1] 5885.94 Forum

#Boxplot 
boxplot(BC1$BC~BC1$Location, ylab="Black carbon", xlab="Location")

hist(BG1$BC)
hist(F1$BC)

#PC07 - particle count
#small
mean(PC_BG$Small) #[1] 60593.33 background
mean(PC_F$Small) #[1] 337314.1 Forum 
sd(PC_BG$Small) #[1] 8657.182 background
sd(PC_F$Small) #[1] 568314.4 Forum

boxplot(PC$Small~PC$Location, ylab="Small particles", xlab="Location")
hist(PC_BG$Small)
hist(PC_F$Small)

#large
mean(PC_BG$Large)  #[1] 6312.593 background
mean(PC_F$Large)   #[1] 18340.58  Forum
sd(PC_BG$Large)    #[1] 2834.675 Background
sd(PC_F$Large)     #[1] 101351.6 Forum 

boxplot(PC$Large~PC$Location, ylab="Large particles", xlab="Location")



#Normality test to prove that our data is parametric: 
s <- glm(BC1$BC~BC1$Location)
shapiro.test(resid(s)) #is parametric because p-value = 0.6554
#Our data is parametric
#visual check if data is parametric: 
hist(resid(s))
qqnorm(resid(s))
qqline(resid(s))

#Significant difference
#non-parametric data Kruskal-Wallis
kruskal.test(BC1$Location~BC1$BC)

#Kruskal-Wallis chi-squared = 4542.1, df = 4449, p-value = 0.1617

kruskal.test(BC2$Location~BC2$BC)

#Kruskal-Wallis chi-squared = 4291.7, df = 4245, p-value = 0.3044




