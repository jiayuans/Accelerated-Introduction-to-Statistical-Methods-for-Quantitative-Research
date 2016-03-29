##1
#a
setwd("/Users/jiayuan/Documents/MA881")
summary = function(x){
  if (is.numeric(x)==FALSE)stop("Invalid input. summary() only takes numeric vectors.")
  return(list(mean(x),median(x),var(x)))
}
z <- c(1,2,3,4,5,6,7,8,9)
summary(z)

#b
f = function(x,n){
  i = 0
  while(i <= n){
    i=i+1
    a <- exp(-x)*(x^i)/factorial(i)
    b <- sum(a)
  }
  print(b)
}
f(2,2)

#c
char = function(x){
  for (i in x) {
    if (is.character(i)){
      print(i)
    }else stop("Invalid input. char() only takes character vectors.")
  }
}
char("hello")

#d
library(ggplot2)
rwalk <- function(k){
  k1 = as.integer(k)
  if(k1 != k)stop("rands() requires an integer value to start.")
  
  i=1
  x=0
  while(x[i] != k){
    if(runif(1)<.5)D = 1
    else D = -1
    i = i+1
    x[i] = x[i-1]+ D
  }
  return(data.frame(x))
}

ser <- data.frame(rwalk(11))
ser
t <- 1:length(ser$x)
qplot(t,x, data=ser, geom="line")


##2
#a
ma3 <- function(x){
  n <- length(x)
  if(n==3){
    (x[1:(n-2)] + x[2:(n-1)] + x[3:n])/3
  }else stop("ma3() requires a vector of length 3.")
}
ma3(c(1,2,3))

#b
mak <- function(x,k){
  n <- length(x)
  i <- 1
  ma <- 0
  while (i<=k){
    ma <- ma + x[i:(n-k+i)]
    i <- i+1
  }
  ma/k
}
mak(c(1:5,6:1),3)

#c
mak(c(1:5,6:1),11)
#There is still a result, 3.273, in this situation.

#d
mak <- function(x,k){
  if(k<length(x)){
    n <- length(x)
    i <- 1
    ma <- 0
    while (i<=k){
      ma <- ma + x[i:(n-k+i)]
      i <- i+1
    }
    ma/k
  }else stop("mak() requires k<length(x).")
}
#>mak(c(1:5,6:1),11)
#Error in mak(c(1:5, 6:1), 11) : mak() requires k<length(x).

#e
mak <- function(x,k){
  if(k<length(x)){
    n <- length(x)
    i <- 1
    ma <- 0
    while (i<=k){
      ma <- ma + x[i:(n-k+i)]
      i <- i+1
    }
    ma/k
  }else stop("mak() requires k<length(x).")
}
mak(c(1:5,6:1),1)
#The result is the same as values in x

##3
fx <- function(x){
  if(x>-4 && x<4){
    ifelse(x < 0, x^2 + 2*x + 3, ifelse(x < 2, x+3, x^2 + 4*x - 7))
  }else stop("Invalid input. fx() only takes values in (-4,4).")
}
fx(2)
x <- seq(-3.99999, 3.99999, len=100)
plot(x, fx(x), type="l")

##4
fx <- function(mat){
  mat[mat%%2 == 1] <- 2 * mat[mat%%2 == 1]
  mat
}
fx(matrix(c(1,1,3,5,2,6,-2,-1,-3),nrow=3,byrow=TRUE))

##5
#a
pois = function(lamda, M){ 
  t = rexp(1, lamda)
  i = 1
  while (t[i] < M) {
    t = c(t, t[i] + rexp(1, lamda))
    i = i + 1
  }
  return(t[-i])
}
pois(1,3)

#b
library(ggplot2)
lengths = numeric(10000)
mean = numeric(10000)
var = numeric(10000)
for (i in 1:10000){
  lengths[i] = length(pois(5, 1))
} 
lengths
hist(lengths, xlab="Vector Lengths")
qplot(lengths, geom = "histogram") 
#The lengths also have a poisson distribution.
mean(lengths) #equals to lamda=5
var(lengths) #equals to lamda=5
qqplot(lengths,rpois(10000,5))
