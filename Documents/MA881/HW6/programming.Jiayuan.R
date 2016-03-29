library(ggplot2)
library(qualityTools)
# create a vector of w exponential waiting times with lambda = lam

set.seed(50)

wait <- function(w,lam){
  a = NULL
  for(i in 1:w){
    a = c(a,rexp(1,rate = lam))
  }
  return(a)
}

#now we plot the waiting time to see if it is exponential
w <- wait(100,2)
w

x <- seq(0,4,.1)

y<-dexp(x,2)

p1 <- qplot(w, binwidth = .2) + geom_line(aes(x,y,color='red'))
p1 
mean(w)
sd(w)
# when we run it we can see mean equla to standar deviation, so it is exponential

###Jiayuan: 
##################################################################################
#another way is to use qqplot to see if we can get a straight line, 
#this time, yes, so w has the same distribution as the Exponential 
qqplot(w,rexp(100,2))
##################################################################################

# now we generate poisson process until time reach M. we use the exponential waiting times above to generate poisson
pois = function(lam, M) {
  out = wait(1,lam)
  len = 1
  while (out[len] < M) {
    #adding values until reach M
    out = c(out, out[len] + rexp(1, lam))
    len = len + 1
  }
  # return value where less than M
  return(out[-len])
}

# in order to check if pois is Poisson distribution, then we generate 10000 of these with lambda=2, and M=1.
#and the lengths of the vector return in each case shoud be Poisson. so we plot it as a Histogram
# and we also calculate the mean and variance, and it turns out the mean=variance=lambda
# Thus, it is Poisson
lens<-numeric(10000)
for (i in 1:10000) lens[i]<-length(pois(2,1))#lam=2,M=1
hist(lens)
mean(lens)
var(lens)

###Jiayuan:
##################################################################################
#qplot the hisgram
qplot(lens, geom = "histogram") 

#again, another way is to use qqplot to see if we can get a straight line, 
#this time, yes, so w has the same distribution as the Poisson 
qqplot(lens,rpois(10000,2))

#check the mean and variance，they are the same as the mean and variance you get above 
mean(rpois(1000,2))   
var(rpois(1000,2)) 
#so it is Poisson for sure
##################################################################################

# Conclusion: if X is Poisson with parameter lambda.then the time until the first arrival is exponential with parameter lambda.

###Jiayuan:
##################################################################################
#I think the conclusion should be if there is a vector of w exponential waiting times with lambda = lam,
#so the number of events have a poisson distribution with parameter lam
##################################################################################

# Now we want to find the relationship between Poisson and gamma distribution
# now simlate the waiting time until k events to occur with lambda = lam

wait.for <- function(k, lam){
  time = 0
  count = 0
  a = NULL
  while(count < k){
    inter=rexp(1,lam)
    count = count + 1
    time = time+inter
  }
  
  return(time) # time should be gamma
} 

###Jiayuan:
##################################################################################
#check: after set seed,
#the total waiting time for 5 events to occur with lambda = 2 with set.seed(50)
#is the same as the sum of interval waiting time, using the function wait() above
set.seed(50)
wait.for(5,2) 
set.seed(50)
sum(wait(5,2)) 
##################################################################################

#Now let's test if we succeed generate the gamma distribution.

gam.test <-function(rep, k, lam ){
  a=NULL
  for (i in 1:rep){
    t = wait.for(k,lam)
    a = c(a,t)
    
  }
  
  return(a)
}
#first, we plot the the gam.test with 1000 times and k=2, lam=2
#then we calculate mean and variance
#we find that mean=k*(1/lam) ,  var=k*(1/lam)^2, Thus it is Gamma distirbution
gamm<-gam.test(1000,2,2)
hist(gamm)

###Jiayuan:
##################################################################################
mean(gamm) #equals to 0.977 = 2/2 = k/lam
var(gamm) #equals to 0.5153 = 5/2^2 = k/lam^2

#qplot the hisgram
qplot(gamm, geom = "histogram") 

#again, another way is to use qqplot to see if we can get a straight line, 
#this time, yes, so w has the same distribution as the Gamma 
qqplot(gamm,rgamma(10000,2,2))

#check the mean and variance，they are the same as the mean and variance you get above 
mean(rgamma(1000,2,2))
var(rgamma(1000,2,2)) 
#so the total waiting time has a Gamma distribution with parameter 2,2, 
#which is the number of events and lambda
##################################################################################

#Conclusion: If X~Poisson(lamba),then the time until k arrivals is Gamma(k,1/ lambda)
