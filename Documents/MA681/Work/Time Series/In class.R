library(itsmr)
library(zoo)
library(lmtest)

plot.ts(LakeHuron)
plot.ts(wine)
ts(wine)
ts(deaths)
plot.ts(deaths)
plot.ts(strikes)
plot.ts(lake)
plot.ts(sunspot.year)
plot.ts(sunspot.month)

library(ggfortify)
wine <- ts(wine)
autoplot(wine)
autoplot(LakeHuron)

x.norm <- rnorm(50)

autoplot(x.norm)
x.ts <- ts(x.norm)
t <- 1:50

p <- autoplot(x.ts)
p
coefficients(lm(x.ts ~ t))
p + geom_abline(intercept=-.31, slope=0)

x.u <- runif(50)

acf(x.ts)

### Lake Huron
hur <- lake
plot.ts(hur)
hur <- ts(hur)
autoplot(hur)
t <- 1:length(hur)
hur.tr <- lm(hur ~ t)
hur.tr$coefficients
hur.pr <- predict(hur.tr)

plot(hur.tr$residuals)
plot.ts(hur.tr$residuals)
var(hur.tr$residuals)
qqnorm(hur.tr$residuals)
qqplot(x = hur.pr, y = hur.tr$residuals) + geom_point()
head(hur.pr)
head(hur.tr$residuals)

dwtest(hur.tr)
autoplot(lake)
acf(hur.tr$residuals)
pacf(hur.tr$residuals)

### accidental deaths
autoplot(deaths)
deaths <- ts(deaths)
autoplot(deaths)
t <- 1:length(deaths)
coefficients(lm(deaths ~ t))
autoplot(deaths) + geom_abline(intercept = 9097, slope = 0)
mean(deaths)
autoplot(deaths) + geom_abline(intercept = 8787, slope = 0)

#### from itsmr
y <- hr(deaths, c(12,6))
plotc(deaths, y)
length(deaths)
length(y)
head(deaths)
head(y)
resid <- deaths - y
autoplot(resid)
acf(resid)


## Nov.10,Tue.
library(itsmr)
is.ts(wine)
plot.ts(wine)
plotc(wine)

library(ggfortify)
autoplot(wine)

wine <- ts(wine, start = c(1980,1), frequency = 12)
autoplot(wine)
plot.ts(wine)
plotc(wine)

dwine0 <- fortify(wine)
head(dwine0)

xv <- c("log")
wine1 <- Resid(wine, xv)
plotc(wine)
plotc(wine1)

dwine1 <- fortify(wine1)
dwine01 <- cbind(dwine0, dwine1$Data)
colnames(dwine01) <- c("Yr", "wine0", "wine1")

head(wine)
head(wine1)
log(464)
wine2 <- log(wine)

autoplot(wine2)
dwine2 <- fortify(wine2)

dwine012 <- cbind(dwine01, dwine2$Data)
head(dwine012)
colnames(dwine012) <- c("Yr", "wine0", "wine1", "wine2")

head(dwine012)

ggplot(data = dwine012, aes(x=Yr, y=wine0)) + geom_line()
ggplot(data = dwine012, aes(x=Yr, y=wine1)) + geom_line()
ggplot(data = dwine012, aes(x=Yr, y=wine2)) + geom_line()

plotc(strikes)
is.ts(strikes)
strikes <- ts(strikes, start = 1951, frequency = 1)
plotc(strikes)

y1 <- smooth.ma(strikes,2)
plotc(strikes)
plotc(y1)

resid1 <- strikes - y1
plotc(resid1)

y2 <- smooth.fft(strikes,.4)
plotc(y2)
resid2 <- strikes - y2
plotc(resid2)

c <- c(1,1,1,1)
f <- filter(strikes,c)
plotc(f)

dif <- diff(strikes)

library(forecast)
fit <- decompose(wine2, type = "additive")
plot(fit)
fit.ma <- ma(wine2, 12)
head(fit$trend, 15)

strikes.1 <- decompose(strikes)
is.ts(strikes)
acf(strikes)
pacf(strikes)
strikes.smooth <- fft(strikes, .25)
