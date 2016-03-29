setwd("/Users/jiayuan/Documents/MA684")
0.6/sqrt((1-0.6^2)/(135-2))
1-pt(8.649,133)
library("psych", lib.loc="~/R/win-library/3.1")
r.con(0.60,135,p=.95,twotailed=TRUE)

fram <- read.csv("FramHSn500.csv", header=TRUE)
attach(fram)
cor.test(AGE[SEXMALE==1],SYSBP[SEXMALE==1])
cor.test(AGE[SEXMALE==0],SYSBP[SEXMALE==0])
(0.134- 0.5534)/(sqrt(1/(220-3)+1/(280-3)))
87090/198
13750/439.8
1-pf(7.604,1,98)

qolstudy <- read.csv("qolstudy(1).csv", header=TRUE)
attach(qolstudy)
cor.test(educyears,qol)
cor.test(income,qol)
cor.test(genhealth ,qol)

reg <- lm(qol ~  educyears)
summary(reg)
summary.aov(reg)
