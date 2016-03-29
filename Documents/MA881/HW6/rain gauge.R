setwd("/Users/jiayuan/Documents/MA881/HW6/raw data")
library(stringr)
myfile <- dir("/Users/jiayuan/Documents/MA881/HW6/raw data/", pattern="\\.txt")
for (i in myfile){
  name <- str_sub(string=i, start=1, end=7)
  temp <- read.csv(file=file.path("/Users/jiayuan/Documents/MA881/HW6/raw data",i), 
                   skip=2, stringsAsFactors=F)
  assign(x=name, value=temp)
}
#combine the 60 .txt files
L1 <- rbind(L.00.01, L.00.02, L.00.03, L.00.04, L.00.05, L.00.06, 
            L.00.07, L.00.08, L.00.09, L.00.10, L.00.11, L.00.12,
            L.01.01, L.01.02, L.01.03, L.01.04, L.01.05, L.01.06, 
            L.01.07, L.01.08, L.01.09, L.01.10, L.01.11, L.01.12,
            L.02.01, L.02.02, L.02.03, L.02.04, L.02.05, L.02.06, 
            L.02.07, L.02.08, L.02.09, L.02.10, L.02.11, L.02.12,
            L.03.01, L.03.02, L.03.03, L.03.04, L.03.05, L.03.06, 
            L.03.07, L.03.08, L.03.09, L.03.10, L.03.11, L.03.12,
            L.04.01, L.04.02, L.04.03, L.04.04, L.04.05, L.04.06, 
            L.04.07, L.04.08, L.04.09, L.04.10, L.04.11, L.04.12)
L1 <- L1[,2:length(L1)]
class(L1[2,2])
colnames(L1) <- c(1:24)
#replace "----", "T   ", and "M   " to different values
L1[L1=="----"] <- 0
L1[L1=="T   "] <- 1e-10
L1[L1=="M   "] <- 0
dat <- as.data.frame(sapply(L1, as.numeric))
#dat[complete.cases(dat), ]
head(dat) #numbers in dat is numeric

#make x contain all the numbers in dat
a <- 0
x <- 0
for (j in 1:1827){
  for (i in 1:24){
    x[a]=dat[j,i]
    a=a+1
  }
}

#sum the consecutive values in x except 0
sum <- 0
flag <- 0
R <- NULL
k=0
for(i in 1:length(x)){
  if(x[i] != 0){
    sum=sum+x[i]
    flag=1
  }
  if(x[i]==0 && flag==1){
    k=k+1
    R[k]=sum
    sum=0
    flag=0
  }
  if(sum!=0 & i==length(x)){
    R[k+1]=sum
  }
}
R

#get rid of the extreme small values 
length(which(R<=1e-8))
R <- R[-c(which(R<=1e-8))]
R
length(R)

#MEM
alpha <- mean(R)^2/var(R)  #0.3613071
lambda <- mean(R)/var(R)  #1.276204
n <- length(R)
qqplot(R,rgamma(n,0.3613071,1.276204)) 
#a straight line
#The distribution for the data is Gamma.

#MLE
minus.likelihood <- function(theta) {-(n*theta[1]*log(theta[2])-
                                         n*lgamma(theta[1])+(theta[1]-1)*
                                         sum(log(R))-theta[2]*sum(R))}
max.likelihood <- nlminb(start=c(0.3613071, 1.276204), obj = minus.likelihood)
max.likelihood$par #0.5461541 1.9291179
qqplot(R,rgamma(length(R),0.5461541,1.9291179)) 
#a straight line
#The distribution for the data is Gamma.
