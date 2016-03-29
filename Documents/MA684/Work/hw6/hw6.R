setwd("/Users/jiayuan/Documents/MA684/hw6")
homeprices <- read.csv("homeprices.csv",header=T)
attach(homeprices)
cor.test(price,size)
cor.test(price,bedrooms)
cor.test(price,age)
cor.test(age,size)


pcor.test(price,bedrooms,size)
pcor.test(price,age,size)


score <- read.csv("EnviroAttitudeTrial.csv",header=T)
attach(score)
head(score)
reg <- lm(enviroscore1~enviroscore0+SexM+intervention)
summary(reg)

reg0i <- lm(enviroscore1~enviroscore0+SexM+intervention+enviroscore0*intervention)
summary(reg0i)

regSi <- lm(enviroscore1~enviroscore0+SexM+intervention+SexM*intervention)
summary(regSi)
