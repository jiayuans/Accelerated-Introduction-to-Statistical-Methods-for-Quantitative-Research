#4
library(ggplot2)
cntl <- c(0.225, 0.262, 0.217, 0.240,0.230, 0.229, 0.235, 0.217)
trt <-c(0.209, 0.205, 0.196, 0.210, 0.202,0.207, 0.224, 0.223, 0.220, 0.201)

qplot(cntl)
qplot(trt)

boxplot(cntl,trt)

summary(cntl)
summary(trt)

# two sample t test
t.test(cntl,trt)

# really??
cntl.n <- length(cntl)
trt.n <- length(trt)
cntl.m <- mean(cntl)
trt.m <- mean(trt)
cntl.v <- var(cntl)
trt.v <- var(trt)

T <- (trt.m - cntl.m)/sqrt((trt.v/trt.n) + (cntl.v/cntl.n))
T

# permutation test

samp <- c(cntl,trt)
l <- length(samp)
trt.n <- length(trt)

dmeanT <- NULL

for(i in 1:1000){
  
  sampler = sample((1:l),trt.n,replace=FALSE)
  trt.s = samp[sampler]
  cntl.s = samp[-sampler]
  dmean.s = mean(trt.s)-mean(cntl.s)
  dmeanT[i] = dmean.s
}

qplot(dmeanT)

f <- ecdf(dmeanT)
quantile(dmeanT,.95)
quantile(dmeanT,.05)
summary(dmeanT)

# observed difference
permutation.t = cntl.m - trt.m
which(dmeanT>permutation.t) #0
#p-value=0 < 0.05
#we can reject H0,
#there is enough evidence to reject the null hypothesis of no difference,
#and conclude that the ten essays were not actually written by Mark Twain.

