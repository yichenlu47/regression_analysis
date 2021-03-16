library(dplyr)
library(uwIntroStats)

setwd("~/Google Drive/UW Seattle/BIOST 515 Biostatistics II/Data")
data <- read.csv("TWINS.csv",header=TRUE) 
names(data)

regress("geometric mean", hwage ~ educ, data)
summary(data$edu)
a <- 1.903 + 0.03510 * 12
exp(a)

#lower
a <- 1.903 + 12 * (0.03510 - 1.96 * 0.01188)
exp(a)

#higher
a <- 1.903 + 12 * (0.03510 + 1.96 * 0.01188)
exp(a)

lm(log(hwage) ~ educ, data)
data2<- data[data$educ == 12,]
mean(log(data2$hwage), na.rm=TRUE)
exp(mean(log(data2$hwage), na.rm=TRUE))
nrow(data2)
sd(log(data2$hwage), na.rm=TRUE)
sample_upper_sd <- mean(log(data2$hwage), na.rm=TRUE) + 1.96 * sd(log(data2$hwage), na.rm=TRUE) * sqrt(1+1/117)

sample_lower_sd <- mean(log(data2$hwage), na.rm=TRUE) - 1.96 * sd(log(data2$hwage), na.rm=TRUE) * sqrt(1+1/117)
exp(sample_upper_sd)
exp(sample_lower_sd)

predict(lm(log(hwage) ~ educ, data), data2, interval="predict")


predict(lm(log(hwage) ~ educ, data), data2, interval="predict")
exp(1.0686)
exp(3.580333)


data3 <- data[data$educ == 0 |data$educ == 12,]
t.test(data$educ, log(data$hwage), paried = TRUE, var.equal = FALSE)
