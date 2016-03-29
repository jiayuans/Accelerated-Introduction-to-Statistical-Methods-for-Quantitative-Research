##Appendix - Computer Output
#1A
setwd("/Users/jiayuan/Documents/MA684/midterm")
tree <- read.csv("SpruceTrees.csv", header=TRUE)
attach(tree)


#1B, 1C
reg.tree <- lm(height ~ diameter)
summary(reg.tree)

#1D
predict(reg.tree,data.frame(diameter=10),interval="predict")
predict(reg.tree,data.frame(diameter=12),interval="predict")

#2A
memory <- read.csv("Elders_2015.csv", header=TRUE)
attach(memory)
head(memory)
summary(age)
sd(age)
summary(iq)
sd(iq)
summary(hippochange)
sd(hippochange)
length(which(sexf==1))
length(which(sexf==0))
length(sexf)

#2B
cor.test(hippochange,age)
cor.test(hippochange,iq)

#2C, 2D
reg.asi <- lm(hippochange ~ age+sexf+iq)
summary(reg.asi)
summary.aov(reg.asi)

#2E
library(QuantPsyc)
lm.beta(reg.asi)

#2F, 2G
reg.asic <- lm(hippochange ~ age+sexf+iq+relevel(factor(exercisegroup),'1'))
summary(reg.asic)
anova(reg.asic,reg.asi)

#2H
library(car)
Anova(reg.asic,type="II")

#2I
reg.int <- lm(hippochange ~ age+sexf+iq+relevel(factor(exercisegroup),'1')
              +relevel(factor(exercisegroup),'1')*age)
summary(reg.int)
