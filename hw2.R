library(dplyr)
library(uwIntroStats)

setwd("~/Google Drive/UW Seattle/BIOST 515 Biostatistics II/Data")
data <- read.table("mri.txt",header=TRUE) 
names(data)

#1 suitable descriptive statistics related to the distribution of atrophy scores by age.
summary(data$atrophy)
summary(data$age)
plot (data$age, data$atrophy, 
     xlab = "Age", ylab = "Atrophy score", pch = ".")
regress("mean", atrophy ~ age, data)
-16.06 + 0.6980 * 90

regress("geometric mean", atrophy ~ age, data)
exp(2.079+0.01923 * 90)
regress("mean", dsst ~ age, data)

(99-65) * 12 

#1 = yes, 2 = no
data2 <- read.csv("FEVdata.csv",header=TRUE) 
names(data2)
summary(data2$SMOKE)
data2$smoke2 = (data2$SMOKE == 1) #smoker
summary(data2$smoke2)


t.test(FEV ~ smoke2, data2, var.equal = TRUE)
t.test(FEV ~ smoke2, data2, var.equal = FALSE)
(0.9266033-(0.4948346))/(1.96*2)
(0.9084253-0.5130126)/(1.964513*2)
2.56614-3.276862
(0.4948-0.9266)/0.1099/2
#regression with naive SE
summary(lm(FEV ~ smoke2, data2))
confint(lm(FEV ~ smoke2, data2))
#regression with robust SE
regress("mean", FEV ~ smoke2, data2)
(0.5166-0.9049)/0.09888
(0.9084253-0.5130126)/((0.5166-0.9049)/0.09888)
