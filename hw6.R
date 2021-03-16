library(dplyr)
library(uwIntroStats)
library(sandwich)
library(aod)
setwd("~/Google Drive/UW Seattle/BIOST 515 Biostatistics II/Data")


###-----QUESTION 1-----###
d <- read.csv("wcgs.csv",header=TRUE)
summary(d$chd69)
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
summary(glm(dibpat_1 ~ chd69_1, data=d1, family = "binomial"))
d1$chd69_2 = 1-d1$chd69_1

summary(glm(dibpat_1 ~ chd69_2, data=d1, family = "binomial"))
d1$chd69_3 = d1$chd69_1 * 10
summary(glm(dibpat_1 ~ chd69_3, data=d1, family = "binomial"))


#-- d --#
178/(178+79) / ( 79/(178+79) )
1411/(1486 +1411) / (1486/(1486 +1411) )
1411/(1486 +1411) / (1486/(1486 +1411) )  / (178/(178+79) / ( 79/(178+79) ))
