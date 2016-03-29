setwd("/Users/jiayuan/Documents/MA684")
distance <- read.csv("stoppingdistance.csv", header=TRUE)
distance
attach(distance)
plot(dist~speed)
speed
reg.results <- lm(dist~speed)
summary(reg.results)
par(mfrow=c(1,1))
plot(reg.results)

predict(reg.results,data.frame(speed=10),interval="predict")
predict(reg.results,data.frame(speed=60),interval="predict")


100.74-2.04*13.5*sqrt(1/30+(((30-27)^2)/((30-1)*(4.5^2))))

csi <- read.csv("CSI femur stature inches.csv", header=TRUE)
attach(csi)
plot(height~femurlength)
mean(femurlength)
sd(femurlength)
mean(height)
sd(height)

reg.results <- lm(height~femurlength)
summary(reg.results)
confint(reg.results,"femurlength")
length(height)

predict(reg.results,data.frame(femurlength=19),interval="predict")
predict(reg.results,data.frame(femurlength=19),interval="confidence")
