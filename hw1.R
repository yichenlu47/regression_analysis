library(dplyr)
library(uwIntroStats)

setwd("~/Google Drive/UW Seattle/BIOST 515 Biostatistics II/Data")
data <- read.csv("stringdata.csv",header=TRUE) 
names(data)
regress("mean", length ~ weight, data)
sd(data$length)
sd(data$weight)
sqrt(0.8273)
sd(data$weight) / sd(data$length) * 0.7506 

#problem 3
wdat<- read.csv("wcgs.csv",header=TRUE) 
names(wdat)
unique(wdat$height)
min(wdat$height)
max(wdat$height)
regress("mean", weight~height,wdat)
plot(wdat$height, wdat$weight, 
     xlab = "Height", ylab = "Weight", pch = ".")
wdat$weight2= wdat$weight/2.205
wdat$height2= wdat$height/2.54
regress("mean", weight2 ~height2,wdat)
wdat.lm = lm(weight~height,wdat)
wdat.lm
wdat.res = resid(wdat.lm)
plot(wdat$height, wdat.res, ylab="Residuals", xlab="Height", pch = ".")
abline(0, 0)   
