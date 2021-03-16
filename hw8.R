library(dplyr)
library(uwIntroStats)
library(survival)
setwd("~/Google Drive/UW Seattle/BIOST 515 Biostatistics II/Data")

###-----QUESTION 1-----###

#time:  the minimum of time to first recurrence or the end of follow-up
#event=1 if time gives time to first recurrence, 0 otherwise
#Group: 1=placebo  2=thiotepa
#futime = follow-up time.  Inidicates how many monhths the patient is followed.
#number = initial number of tumors.
#size = Size of initial tumors.  Measured in centimeters.
#rt1 = time (in months) that first tumor recurrence was observed.

k <- read.delim("TUMORdata.txt",header=TRUE)
names(k)
#log-ranl
survdiff(Surv(time, event) ~ group, data = k)
#kaplan-meier
survobj <- with(k, Surv(time, event))
kms <- survfit (survobj ~ group, data=k)
plot(kms, mark.time=TRUE,
     col=c("blue", "coral"),
     xlab="time to first recurrence",
     ylab="Survival",
     main="Kaplan-Meier survival curves")
legend("topright", lty=1,
       col=c("blue", "coral"),
       legend=c("placebo", "thiotepa" ))


###-----QUESTION 2-----###
summary(coxph(formula = survobj ~ group, data = k))
###-----QUESTION 3-----###
#Group: 1=placebo  2=thiotepa change to 1=placebo  0=thiotepa
k$GX = (k$group == 1) 
summary(coxph(formula = survobj ~ GX, data = k))




###-----QUESTION 4-----###
d <- read.csv("pbc.csv",header=TRUE)
names(d)
levels(d$status)
d$event = (d$status == "Dead")
survobj <- with(d, Surv(years, event))
summary(coxph(formula = survobj ~ albumin, data = d))

exp((3.5-2.5) * -1.7957)
1/exp((3.5-3) * -1.7957)

