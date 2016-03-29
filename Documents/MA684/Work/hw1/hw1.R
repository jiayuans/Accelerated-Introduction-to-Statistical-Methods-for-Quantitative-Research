#1
#a
a=c(22 , 36 , 35 , 47 ,  36 , 34 , 13)
b=c(23 , 20 , 11 ,   9 ,  31 , 34)
ma=mean(a)
mb=mean(b)
sda=sd(a)
sdb=sd(b)

#2
dietstudy <-read.csv("/Users/jiayuan/Documents/MA684/dietstudy.csv", header = TRUE)
dietstudy 
dietA<-dietstudy$WtLoss[1:7]
dietB<-dietstudy$WtLoss[8:13]
mean(dietA)
mean(dietB)
sd(dietA)
sd(dietB)
t.test(dietA) #95 percent confidence interval: 21.64987 42.06442
t.test(dietstudy$WtLoss ~ dietstudy$Diet,var.equal=TRUE)
#t = 1.7758, df = 11, p-value = 0.1034
#Interpret: The calculated p-value is bigger than 0.05, so we fail to reject the null hypothesis.
#There is enough envidence to conclude that the mean weight loss on Diet A and Diet B are similar.


