library(dplyr)
library(uwIntroStats)
library(sandwich)

setwd("~/Google Drive/UW Seattle/BIOST 515 Biostatistics II/Data")


###-----QUESTION 1-----###
mri1 <- read.table("mri.txt",header=TRUE) 
names(mri1)

##-----a-----#
plot(mri1$age, mri1$sbp,
     xlab="Age", ylab="SBP", main="", 
     col= ifelse(mri1$male==1, "slateblue", "coral"), pch=16)
# lowess smooth
mri1_male = subset(mri1, male==1)
mri1_female = subset(mri1, male==0)
smooth_male <- with(mri1_male, lowess(mri1_male$age,mri1_male$sbp, iter=0))
smooth_female <- with(mri1_female, lowess(mri1_female$age,mri1_female$sbp, iter=0))
lines(smooth_male, col="slateblue", lwd=2)
lines(smooth_female, col="coral", lwd=2)
0.7372 - 0.5709
##-----c-----#
regress("mean", sbp ~ age * male, mri1)


###-----QUESTION 1-----###

##-----b-----#
regress("mean", sbp ~ age + as.factor(race), mri1)

99.01+4.344
99.01-0.5706  
99.01+6.345



###-----QUESTION 3-----###
mod = lm(sbp ~ age + as.factor(race) + age * male, mri1)
mod
76.36 + 0.7302 * 70 + 42.29 * 0 - 0.5756 * 70 * 0 + 4.292 * 1 - 0.9024  * 0 + 6.583 * 0
76.36 + 0.7302 * 70 + 4.292
## use a classical linear regression
mri1_subgroup = mri1_female %>% filter(age == 70) %>% filter(race == 2)

pred = predict(mod, mri1_subgroup, interval="prediction")
pred
##-----c-----#
resid.student <- rstudent(mod)
sbp.fitted <- fitted.values(mod)
plot(sbp.fitted, resid.student, 
     xlab = "Fitted SBP", ylab = "Studentized Residual",
cex.lab= 1, col = "coral")


qqnorm (resid.student,  
        ylab = "Studentized Residual", xlab = "Normal Scores",
        cex.lab= 1, col = "slateblue", pch = 16, cex=0.3)
qqline(resid.student, col = "yellow")
