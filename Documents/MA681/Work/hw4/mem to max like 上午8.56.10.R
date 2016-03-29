setwd("/Users/jiayuan/Documents/MA681/hw4")
library(ggplot2)
data <- read.csv("illinois rain 1960-1964.csv",header=F)
data1 <- unlist(data)
head(data1)
tail(data1)
data1 <- data.frame(data1[1:227])
colnames(data1) <- "x"

qplot(x, data=data1, geom = "histogram",binwidth=.3)


mean(data1$x)
var(data1$x)


alpha <- mean(data1$x)^2/var(data1$x)
alpha
lambda <- mean(data1$x)/var(data1$x)
lambda

# Homework -- produce a density plot of the 
# gamma distribution with these parameters
# see http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
# for instructions on how to plot

ggplot(data1, aes(x=data1$x)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

## homework # 2
## now bootstrap -- samples (n=227) from gamma(alpha, lambda)
## we get variance for alpha and lambda
## note lambda = mean/var, alpha = mean^2/var

obs<-rgamma(227, shape=alpha, rate = lambda)
obs
th.hat<-median(obs)
obs.n <- length(obs)
B <- 1000
Tboot <- rep(0,B)
for(i in 1:B){
  obs.s <- sample(obs, obs.n, replace=TRUE)
  Tboot[i] <- median(obs.s)
}
se <- sqrt(var(Tboot))
se
alpha.boot <- (th.hat^2)/(se^2)
alpha.boot
lambda.boot <- th.hat/(se^2)
lambda.boot
## 



###################
##  max likelihood


# try nlminb

func <- function(y){(y[1]-3)^2 + (y[2]+1)^2}
min.func <- nlminb(start=c(1,1), obj= func)
min.func$par
# comes up with the obvious answer


# now lets use it to get max likelihood
x1 <- data1$x

n <- length(data1$x)
# remember we know how to MINIMIZE so
# setup theta <- c(alpha,lambda)
# and 

minus.likelihood <- function(theta) {-(n*theta[1]*log(theta[2])-n*lgamma(theta[1])+(theta[1]-1)*sum(log(x1))-theta[2]*sum(x1))}

max.likelihood <- nlminb(start=c(.3762, 1.6767), obj = minus.likelihood)

max.likelihood$par


# homework bootstrap to get standard errors for alpha and lambda





