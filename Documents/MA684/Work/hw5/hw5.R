setwd("/Users/jiayuan/Documents/MA684/hw5")
depression <- read.csv("depression2012.csv",header=T)
attach(depression)
lm1 <- lm(dep_symptoms~age+sexmale+yrsed+drinks)
summary(lm1)


lighter <- NULL
lighter[drinkcat==1] <- 1
lighter[drinkcat==0 | drinkcat==2] <- 0
heavier <- NULL
heavier[drinkcat==2] <- 1
heavier[drinkcat==0 | drinkcat==1] <- 0
lm2 <- lm(dep_symptoms~age+sexmale+yrsed+lighter+heavier)
summary(lm2)


lm.full <- lm(dep_symptoms~age+sexmale+yrsed+lighter+heavier)
summary(lm.full)
summary.aov(lm.full)
lm.reduced <- lm(dep_symptoms~age+sexmale+yrsed)
summary(lm.reduced)
summary.aov(lm.reduced)


lm3 <- lm(dep_symptoms~age+sexmale+yrsed+relevel(factor(drinkcat),'2'))
summary(lm3)
anova(lm.full,lm.reduced) 
Anova(lm.full,type="II") 

