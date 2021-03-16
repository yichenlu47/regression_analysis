library(dplyr)
library(uwIntroStats)
library(sandwich)

setwd("~/Google Drive/UW Seattle/BIOST 515 Biostatistics II/Data")


#QUESTION 1
smsa <- read.csv("SMSA.csv",header=TRUE) 
names(smsa)
plot(smsa$Mortality, log(smsa$NOx),
     xlab="log(NOx)", ylab="Mortality", main="", 
     col= adjustcolor("black", alpha=0.2), pch=19)
# lowess smooth
l.smooth <- with(smsa, lowess(smsa$Mortality, log(smsa$NOx), iter=0))
# add this smoother, and a legend
lines(l.smooth, col="blue", lwd=2)
legend("bottomright","Lowess smoother", col="blue", lwd=2, bty="n")


#QUESTION 2
regress("mean", Mortality ~ log(smsa$NOx), smsa)
#QUESTION 3
smsa2 <- smsa %>% mutate(low_rainfall = 1*(Rain < 20))
summary(smsa2$low_rainfall)
lm(Mortality ~ log(smsa$NOx) + low_rainfall, smsa2)
regress("mean", Mortality ~ log(NOx) + low_rainfall, data=smsa2)

#QUESTION 4
plot(smsa2$Mortality, log(smsa2$NOx),
     xlab="log(NOx)", ylab="Mortality", main="", 
     col= adjustcolor(ifelse(smsa2$low_rainfall == 1, "black", "red"), alpha=1), pch=19)

#QUESTION 5

regress("mean", Mortality ~ log(NOx) + Rain, data=smsa2)
regress("mean", Mortality ~ log(NOx) + Rain + log(NOx) * Rain, data=smsa2)

#QUESTION 7
888.9 - 0.4785 * 15
- 22.70 + 1.463 * 15

888.9 - 0.4785 * 36
- 22.70 + 1.463 * 36

#QUESTION 8
mri <- read.csv("MRI2.csv",header=TRUE) 
names(mri)
plot(mri$height, mri$weight,
     xlab="Height", ylab="Weight", main="", 
     col= adjustcolor("black", alpha=0.2), pch=19)
summary(mri$male)
mri_male <- mri[mri$male == 1,]
mri_female <- mri[mri$male == 0,]
abline(lm(mri$weight ~ mri$height))
abline(lm(mri_male$weight ~ mri_male$height), col = "blue")
abline(lm(mri_female$weight ~ mri_female$height), col = "red")

regress("mean", weight ~ height, data=mri)
regress("mean", weight ~ height + male, data=mri)
regress("mean", weight ~ height + male + height * male, data=mri)

#question 8d
mean(mri_male$weight)
mean(mri_female$weight)
mean(mri_male$height)

#question 8f
-105.3 + 3.757 
