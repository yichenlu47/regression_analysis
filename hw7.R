library(dplyr)
library(uwIntroStats)
library(sandwich)
library(aod)
setwd("~/Google Drive/UW Seattle/BIOST 515 Biostatistics II/Data")

###-----QUESTION 1-----###
k <- read.csv("prison_data.csv",header=TRUE)
names(k)

#RESPONSE........Misconduct Violation (1) or not (0)
#SCORE...........Classification Score
#STRIKE 2........Two Striker Inmate (1) or not (0)
#STRIKE 3........Three Striker Inmate (1) or not (0)
#TREAT...........Classified to Level 4 (1) or not (0)

regress("odds", response ~ score, k)
exp(-1.289 + 0.0125 * 25)/(1+exp(-1.289 + 0.0125 * 25))
exp(-1.289 + 0.0125 * 50)/(1+exp(-1.289 + 0.0125 * 50))
exp(-1.289 + 0.0125 * 75)/(1+exp(-1.289 + 0.0125 * 75))

regress("odds", response ~ score + treat, k)
exp(-1.504 + 0.02386 * 25)/(1+exp(-1.504 + 0.02386 * 25))
exp(-1.504 + 0.02386 * 50)/(1+exp(-1.504 + 0.02386 * 50))
exp(-1.504 + 0.02386 * 75)/(1+exp(-1.504 + 0.02386 * 75))
exp(-1.504 + 0.02386 * 25 -0.7317)/(1+exp(-1.504 + 0.02386 * 25 -0.7317))
exp(-1.504 + 0.02386 * 50 -0.7317)/(1+exp(-1.504 + 0.02386 * 50 -0.7317))
exp(-1.504 + 0.02386 * 75 -0.7317)/(1+exp(-1.504 + 0.02386 * 75 -0.7317))

-1.654 + 1.336
0.03003 -0.03558 
exp(-0.00555 - 1.96*3.279 * 10^(-3))
exp(-0.00555 + 1.96*3.279 * 10^(-3))

###-----QUESTION 9-----###
d <- read.csv("birthdata.csv",header=TRUE)
d$low_bwt = (d$bwt < 2500)
d1 = subset(d, parity==0 & smoker=="N" & drinker=="N")
levels(d1$gender)
d1$male = (d1$gender == "M")
summary(d1$male)
summary(d$low_bwt)
summary(d$smoker)
regress("odds", low_bwt ~ age + education + as.factor(race) + as.factor(male), d1)

#mother's age as the predictor of interest, 
#and adjusting for race, mother's education, and the baby's sex

summary(d2$age)
d1 <- d%>% mutate(chd69_1 = ifelse(chd69 == "yes", 1, 0)) 
regress("odds", chd69_1~age, data=d1)
d2 <- d1 %>% mutate(chd69_est = -5.940 + 0.07442 * age) 
plot(d2$age, d2$chd69_est,
     xlab = "Age", ylab = "log-odds of having CHD")

d3<-d2 %>% mutate(chd69_prob = exp(chd69_est)/(1+ exp(chd69_est)))

plot(d3$age, d3$chd69_prob,
     xlab = "Age", ylab = "probability of having CHD")
names(d)

#odds
h40 <- exp(-5.940 + 0.07442 * 40)
h45 <- exp(-5.940 + 0.07442 * 45)
h50 <- exp(-5.940 + 0.07442 * 50)
h55 <- exp(-5.940 + 0.07442 * 55)
h60 <- exp(-5.940 + 0.07442 * 60)

#probability
h40/(1+h40)
h45/(1+h45)
h50/(1+h50)
h55/(1+h55)
h60/(1+h60)

#odds ratio
exp(-5.940 + 0.07442 * 45)/exp(-5.940 + 0.07442 * 40)
exp(-5.940 + 0.07442 * 50)/exp(-5.940 + 0.07442 * 45)
exp(-5.940 + 0.07442 * 55)/exp(-5.940 + 0.07442 * 50)
exp(-5.940 + 0.07442 * 60)/exp(-5.940 + 0.07442 * 55)

#risk ratio
h45/(1+h45) /(h40/(1+h40))
h50/(1+h50) /(h45/(1+h45))
h55/(1+h55) /(h50/(1+h50))
h60/(1+h60) /(h55/(1+h55))

#-- e --#
d1$age_5 = d1$age/5
regress("odds", chd69_1~age_5, data=d1)

#-- g--#
library(aod)
lreg1<-glm(chd69_1~age,
           data=d1,family="binomial")
lreg0<-glm(chd69_1~1,
           data=d1,family="binomial")
anova(lreg0, lreg1, test= "LRT")

#---- 2 ----#
#-- a --#
t <- table(d1$chd69_1, d1$dibpat)
t
chisq.test(t)
178/(178+1411)
79/(1486+79)

#odds
178/(178+1411)/(1411/(178+1411))
79/(1486+79)/(1486/(1486+79))

(1486/(1486+79))/(79/(1486+79))

#odds ratio
178/(178+1411)/(1411/(178+1411)) / (  79/(1486+79)/(1486/(1486+79))  )

#-- b --#
regress("odds", chd69_1~dibpat, data=d1)
(  79/(1486+79)/(1486/(1486+79))  )/ (  178/(178+1411)/(1411/(178+1411))    )
1-0.4214
1- 0.3201
1- 0.5548


#-- c --#
d1$dibpat_1 = ifelse(d1$dibpat == "A1,A2", 0, 1)
regress("odds", dibpat_1 ~ chd69_1, data=d1)
d1$dibpat

#-- d --#
178/(178+79) / ( 79/(178+79) )
1411/(1486 +1411) / (1486/(1486 +1411) )
1411/(1486 +1411) / (1486/(1486 +1411) )  / (178/(178+79) / ( 79/(178+79) ))
