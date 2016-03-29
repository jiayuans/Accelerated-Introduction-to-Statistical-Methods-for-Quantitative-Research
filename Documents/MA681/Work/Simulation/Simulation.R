x <- 1:10
n <- rep(15,10)
p <- exp(x-5)
p <- p/(1+p)
p

y<- rbinom(10,n,p)
plot(x,y/n,xlab = "dose", ylab = "proportion who did")

N <- 10000
p <- matrix(0,N,10)
head(p)
check <- rep(0,N)
for (i in 1:10){
  p[,i] <- rbeta(N,y[i]+1,n[i]-y[i]+1)
}
head(p)

for (i in 1:N){
  check[i] <- min(diff(p[i,]))
}

good <- (1:N)[check >= 0]
head(good)

p <- p[good,]

N <- nrow(p)
delta <- rep(0,N)

for (i in 1:N){
  temp <- cumsum(p[i,])
  j <- (1:10)[temp > .5]
  j <- min(j)
  delta[i] <- x[j]
}

print(mean(delta))

left <- quantile(delta, .025)
right <- quantile(delta, .975)

print(c(left,right))

dim(p)

print(N)
matplot(x,t(p),ylab = "",xlab = "dose")
plot(table(delta)/N, type="h",lwd = 3,xlim=c(1,10))
