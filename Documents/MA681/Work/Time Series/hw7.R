### Jiayuan Shi
### MA881 Assignment7: Time Series

## 1
library(itsmr)
library(MASS)
library(datasets)

# Estimation and Elimination of Both Trend and Seasonality
# Method S1: Estimation of Trend and Seasonal Components
plotc(accdeaths)
y1 <- season(accdeaths, 12)
deseasonalized.death <- accdeaths - y1
# Plot1: The deseasonalized monthly accidental deaths data
plotc(deseasonalized.death)
# Plot2: The estimated seasonal component of the monthly accidental deaths data
y1 <- ts(y1, start=c(1973,1), frequency = 12)
plotc(y1)

# Method S2: Elimination of Trend and Seasonal Components by Differencing
# Plot3 & 4: The differenced series derived from the monthly accidental deaths
d <- diff(accdeaths, lag=12)
plotc(d)
d1 <- diff(d, lag=1)
plotc(d1)

## 2
# AR(1), ARMA(1,0)
ARMA01 <- arima.sim(model=list(ar=c(0.9)), n=100) 
# MA(1), ARMA(0,1)
ARMA10 <- arima.sim(model=list(ma=c(0.2)), n=100)
# ARMA(1,1)
ARMA11 <- arima.sim(model=list(ar=c(0.9),ma=c(0.2)), n=100)
# ARMA(1,2)
ARMA12 <- arima.sim(model=list(ar=c(0.9),ma=c(0.2,-0.9)), n=100)
# ARMA(2,1)
ARMA21 <- arima.sim(model=list(ar=c(0.9,-0.2),ma=c(0.2)), n=100)
# ARMA(2,2)
ARMA22 <- arima.sim(model=list(ar=c(0.9,-0.2),ma=c(0.2,-0.9)), n=100)
# ARMA(2,3)
ARMA23 <- arima.sim(model=list(ar=c(0.9,-0.2),ma=c(0.2,-0.9,0.1)), n=100)

acf(ARMA01)
acf(ARMA10)
acf(ARMA11)
acf(ARMA12)
acf(ARMA21)
acf(ARMA22)
acf(ARMA23)

## 3
plotc(JohnsonJohnson) # No Seasonality 
plotc(UKgas) # Seasonality 
plotc(presidents) # No Seasonality 

# JohnsonJohnson:
jstl = stl(JohnsonJohnson, s.window="periodic")
plot(jstl, main = "STL decomposition for JohnsonJohnson data")

library(forecast)
# noise
M=c("log","season",12,"trend",1)
e <- Resid(JohnsonJohnson,M)
test(e)
# ARIMA model for Noise 
a = arma(e,p=4,q=3)
ee <- Resid(JohnsonJohnson,M,a)
test(ee)

# UKgas:
ustl = stl(UKgas, s.window="periodic")
plot(ustl, main = "STL decomposition for UKgas data")

# noise
M=c("log","season",12,"trend",1)
e <- Resid(UKgas,M)
test(e)
# ARIMA model for Noise 
a = arma(e,p=4,q=3)
ee <- Resid(UKgas,M,a)
test(ee)

# presidents:
newp <- na.interp(presidents)
plotc(presidents)
plotc(newp) # continuous new presidents data

pstl = stl(newp, s.window="periodic")
plot(pstl, main = "STL decomposition for new presidents data")

# noise
M=c("log","season",12,"trend",1)
e <- Resid(newp,M)
test(e)
# ARIMA model for Noise
a = arma(e,p=4,q=3)
ee <- Resid(newp,M,a)
test(ee)

# autocorrelation function 
acf(presidents, na.action = na.contiguous) 
acf(newp)
# partial autocorrelations
pacf(presidents, na.action = na.contiguous) 
pacf(newp)

ndiffs(x=newp)

# Fit best ARIMA model for Noise
preBest <- auto.arima(newp)
acf(preBest$residuals)
pacf(preBest$residuals)

coef(preBest)

