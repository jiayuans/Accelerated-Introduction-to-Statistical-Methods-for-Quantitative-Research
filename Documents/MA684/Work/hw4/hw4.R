setwd("/Users/jiayuan/Documents/MA684/hw4")
qolstudy <- read.csv("qolstudy.csv", header=TRUE)
attach(qolstudy)
reg <- lm(qol~age+sexfemale+educyears+income+married)
summary(reg)
confint(reg,"age")
lm.beta(reg)
predict(reg,data.frame(age=30,sexfemale=0,married=0,educyears=16,income=50),interval='prediction')
predict(reg,data.frame(age=60,sexfemale=1,married=1,educyears=16,income=50),interval='confidence')


fram <- read.csv("framingham_n_200.csv", header=TRUE)
attach(fram)
table(CURSMOKE)
t.test(x = SYSBP[CURSMOKE==c(0)], y = SYSBP[CURSMOKE==c(1)],alternative = c("two.sided"),
       var.equal = TRUE,conf.level = 0.95)
sd(SYSBP[CURSMOKE==c(0)])
reg <- lm(SYSBP~CURSMOKE)
summary(reg)

reg <- lm(SYSBP~AGE+SEX+BMI+CURSMOKE)
summary(reg)
