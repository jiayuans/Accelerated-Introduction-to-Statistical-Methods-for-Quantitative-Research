### Appendix 
## R Code and Output for MA684 Final Project 2015
# Jiayuan Shi

library(psych)
library(GPArotation)
library(ltm)

#1A
survey <- read.csv("VoterValues2015.csv",header=T)
survey <- survey[-c(231:234),]
attach(survey)

princomp(~PrivOwn+GayMarriage+Abortion+GovResp+Compete+AssitSuicide,
         cor=TRUE)
responses <- data.frame(PrivOwn,GayMarriage,Abortion,GovResp,Compete,AssitSuicide)
principal(responses,nfactors=2,rotate="varimax")

#1B
moral <- data.frame(GayMarriage,Abortion,AssitSuicide)
political <- data.frame(PrivOwn,Compete)
alpha(moral)$total[[1]]
alpha(political)$total[[1]]

#1C
library(dplyr)
region1 <- subset(survey, Region==1)
region2 <- subset(survey, Region==2)

r1.political <- with(region1, PrivOwn + Compete)
r2.political <- with(region2, PrivOwn + Compete)
t.test(r1.political, r2.political, var.equal = TRUE) # test Political Values

#2A
LeadStudy <- read.csv("LeadStudy2015(1).csv",header=T)
attach(LeadStudy)
table(lead)
mean(age[lead==0])
sd(age[lead==0])
mean(age[lead==1])
sd(age[lead==1])
table(sexF,lead)
table(race,lead)

lowlead <- subset(LeadStudy, lead=="0")
highlead <- subset(LeadStudy, lead=="1")
# calculate p-value for age
t.test(lowlead$age, highlead$age, var.equal = TRUE)
# calculate p-value for sex
chisq.test(table(sexF, lead), correct = FALSE)
# calculate p-value for race
chisq.test(table(race, lead), correct = FALSE)

#2B
table(ADHD)
mean(iq)
sd(iq)

#2C
length(iq[lead==1])
length(iq[lead==0])
mean(iq[lead==1])
sd(iq[lead==1])
mean(iq[lead==0])
sd(iq[lead==0])
t.test(iq ~ lead, var.equal = TRUE)

#2D
reg4 <- lm(iq ~ age + sexF + relevel(factor(race), ref = "1") + lead)
summary(reg4)

#2F
table(lead, ADHD)
chisq.test(table(lead, ADHD), correct = FALSE)

#2G
log.out <- glm(ADHD ~ age + sexF + relevel(factor(race), ref = "1") + lead, 
                family=binomial(link=logit))
summary(log.out)
exp(coef(log.out))
exp(confint(log.out))

library(epicalc)
lroc(log.out)$auc

