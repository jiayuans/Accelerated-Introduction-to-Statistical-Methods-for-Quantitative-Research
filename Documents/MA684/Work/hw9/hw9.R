##HW9 Appendix
#2A
hyperstudy <- read.csv("hyperstudy.csv",header=T)
attach(hyperstudy)
table(hypertension)
147/(147+533) 

#2B
log.out <- glm(hypertension ~ age + smoke + relevel(factor(race),ref='1'),
               family=binomial(link=logit))
summary(log.out)
exp(coef(log.out))
exp(confint(log.out))

#2C
library("epicalc")
lroc(log.out)$auc

#2E
log.reduced <- glm(hypertension ~ age + smoke,
                   family=binomial(link=logit))
summary(log.reduced)

#3A
log.int <- glm(hypertension ~ age + factor(race) + smoke + age*smoke,     
               family = binomial(link=logit))
summary(log.int)

#3C
1-pchisq(7.30 ,8)
