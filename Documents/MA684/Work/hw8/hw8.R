##HW8 Appendix 
#3A
votesurv <- read.csv("votesurv2015(1).csv",header=T)
attach(votesurv)
head(votesurv)
vote[vote==9] <- NA
table(vote)

#3B
293/(293+151) 
.6599+1.96*sqrt((0.6599*(1-0.6599))/(151+293))
length(which(is.na(vote)==TRUE))
length(vote)

#3C
table(vote,sexf)
chisq.test(table(vote,sexf),correct=TRUE)

#3D
library(epitools)
oddsratio.wald(table(vote,sexf)) 

#3E
log.out <- glm(vote ~ age + sexf + relevel(factor(party),ref='1'), 
               family=binomial(link=logit)) 
summary(log.out)
exp(coef(log.out))
exp(confint(log.out))

