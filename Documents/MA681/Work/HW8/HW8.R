# Problem 1
library(ggplot2)
x=seq(0,1.5,length=100)
y=dbeta(x,14,39)
z=dbeta(x,64,45)
dat1 <- data.frame(x,y,z)
ggplot(data=dat1, aes(x,y))+geom_line()
ggplot(data=dat1, aes(x,y))+geom_line()+geom_line(data=dat1,aes(x,z),color="red")

# Problem 2
a=seq(0,1.5,length=100)
b=dgamma(a,14,6)
c=dgamma(a,15,6)
d=dgamma(a,16,6)
dat1 <- data.frame(a,b,c,d)
ggplot(data=dat1, aes(a,b))+geom_line()
ggplot(data=dat1, aes(a,b))+geom_line()+
  geom_line(data=dat1,aes(a,c),color="red")+
  geom_line(data=dat1,aes(a,d),color="green")

