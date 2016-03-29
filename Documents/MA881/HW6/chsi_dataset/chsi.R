setwd("/Users/jiayuan/Documents/MA881/HW6/chsi_dataset")
library(ggplot2) 
health <- read.csv("RELATIVEHEALTHIMPORTANCE.csv",header=T)
names(health) <- c('SC', 'CC', 'CN', 'SN', 'SA', 'SIN', 'LBW', 'VLBW', 'Premature', 'U18', 'O40', 'Unmarried',
                    'LateCare', 'IM', 'IMWNH', 'IMBNH', 'IMH', 'IMN', 'IMPN', 'BrstCan', 'ColCan', 'CHD', 
                   'Homicide', 'LungCan', 'MVA', 'Stroke', 'Suicide', 'Injury')
attach(health)
summary(health)
Worcester.health <- health[which(CN=="Worcester"),][2,]
Worcester.health
num <- 0
for (i in 1:8){
  num[i] <- length(which(Worcester.health==i))
}
num
#5: 11, 'Represent ''Favorable to peers and favorable the U.S. Rate' in the indicator columns
#6: 1, 'Represent 'Favorable to peers and unfavorable the U.S. Rate' in the indicator columns
#7: 6, 'Represent 'Unfavorable to peers and favorable the U.S. Rate' in the indicator columns
#8: 4, 'Represent 'Unfavorable to peers and unfavorable the U.S. Rate' in the indicator columns

#variables of interest:
#SC, CC
#VLBW, IM, CHD, LungCan
rhi <- cbind(SC, CC, VLBW, IM, CHD, LungCan)
head(rhi)
#drop the data of the two situations:  no data available(-2) and no report(-1)
delete=which(VLBW=="-1" | VLBW=="-2" | IM=="-1" | IM=="-2" | CHD=="-1" | CHD=="-2" | LungCan=="-1" | LungCan=="-2")
rhi=rhi[-c(delete),]
#new data frame of the six variables after droping the useless data
rhidata=as.data.frame(rhi)
attach(rhidata)

#compare the four varibales VLBW, IM, CHD, LungCan for differnt counties within MA state
MA <- rhidata[which(SC=="25"),] #MA has the state identifier of 25 in US
MA 
nrow(MA) #14 counties in MA
Worcester <- MA[which(MA$CC=="27"),] #Worcester has the county identifier of 27 in MA
Worcester
#    SC CC VLBW IM CHD LungCan
#1150 25 27    5  5   5       7
#RHI_VLBW_Ind is 5, 
#representing 'Favorable to peers and favorable the U.S. Rate'
#RHI_Infant_Mortality_Ind is 5, 
#representing 'Favorable to peers and favorable the U.S. Rate'
#RHI_CHD_Ind is 5, 
#representing 'Favorable to peers and favorable the U.S. Rate'
#RHI_Lung_Cancer_Ind is 7
#representing 'Unfavorable to peers and favorable the U.S. Rate'
MA.VLBW <- MA$VLBW
MA.IM <- MA$IM
MA.CHD <- MA$CHD
MA.LungCan <- MA$LungCan

mysummary <- function (x) {
  options (digits=4)
  matrix (c( mean(x), sd(x), length(x), min(x), 
             quantile(x,.25), median(x),
             quantile(x,.75), max(x), sum(is.na(x))), ncol=9,
          dimnames = list (NULL, c( "Mean", "SD", "n", "Min", 
                                    "1st Qu", "Median", "3rd Qu", "Max", "NAs")))}

#summaries of MA dataset
summary (MA[,3:6])
mysummary(MA.VLBW)
mysummary(MA.IM)
mysummary(MA.CHD)
mysummary(MA.LungCan)

#stem-plot of MA dataset
stem(MA.VLBW) 
stem(MA.IM) 
stem(MA.CHD) 
stem(MA.LungCan)

#plot of MA dataset
par (mfrow=c(1,2))
plot(MA.VLBW)
plot(MA.IM)
plot(MA.CHD)
plot(MA.LungCan)

#histgram of MA dataset
hist(MA.VLBW)
hist(MA.IM)
hist(MA.CHD)
hist(MA.LungCan)

#boxplot of MA dataset
boxplot(MA.VLBW)
boxplot(MA.IM)
boxplot(MA.CHD)
boxplot(MA.LungCan)

#qplot of MA dataset
qplot(MA.VLBW,data = MA, geom = "histogram")
qplot(MA.IM,data = MA, geom = "histogram")
qplot(MA.CHD,data = MA, geom = "histogram")
qplot(MA.LungCan,data = MA, geom = "histogram")

MAdata = MA[,3:6]
round(cor(MAdata),1) #correlations between two variables
library (corrplot)
par (mfrow=c(1,1))
corrplot (cor(MAdata)) #plot the correlations

#Scree plot to deterine the number of clusters
MAwss <- (nrow(MAdata)-1)*sum(apply(MAdata,2,var))
for (i in 2:8) {
  MAwss[i] <- sum(kmeans(MAdata,centers=i)$withinss)
}   
plot(1:8, MAwss,type="l", xlab="Number of Clusters",ylab="Within groups sum of squares")
#The location of the elbow in the resulting plot suggests 
#a suitable number of clusters for the kmeans is 3
MAK3 <- kmeans(x = MA[,3:6], centers = 3)
MAK3
#K-means clustering with 3 clusters of sizes 5, 4, 5
#Worcester is in the 3rd clustering vector
#Cluster means:
#  VLBW   IM CHD LungCan
#3  5.4 5.00 5.0    7.40
#In the cluster, Worcester's VLBW and LungCan is less than the mean of VLBW and LungCan,
#but it has the same IM and CHD as the mean of IM and CHD, 
#which means for VLBW, Worcester is the same favorable to peers, but more favorable the U.S. Rate 
#than the other counties in MA;
#for IM and CHD, Worcester and other counties in MA are all favorable to peers 
#and favorable the U.S. Rate;
#for LungCan, Worcester is the same unfavorable to peers, but more favorable the U.S. Rate 
#than the other counties in MA
#Within cluster sum of squares by cluster:
#[1]  7.6 16.5  4.4
#the 3rd cluster has the smallest within cluster sum of squares,
#which means the individual indicators in Worcester's group has the samllest differences  

require(useful)
plot(MAK3, data = MA)

#compare the four varibales VLBW, IM, CHD, LungCan for differnt counties within all the states
Worcester <- MA[which(MA$CC=="27"),]
Worcester
#    SC CC VLBW IM CHD LungCan
#1150 25 27    5  5   5       7

mysummary <- function (x) {
  options (digits=4)
  matrix (c( mean(x), sd(x), length(x), min(x), 
             quantile(x,.25), median(x),
             quantile(x,.75), max(x), sum(is.na(x))), ncol=9,
          dimnames = list (NULL, c( "Mean", "SD", "n", "Min", 
                                    "1st Qu", "Median", "3rd Qu", "Max", "NAs")))}
#summaries of rhidata dataset
summary (rhidata[,3:6])
mysummary(VLBW)
mysummary(IM)
mysummary(CHD)
mysummary(LungCan)

#plot of rhidata dataset
par (mfrow=c(1,1))
plot(VLBW)
plot(IM)
plot(CHD)
plot(LungCan)

#histgram of rhidata dataset
hist(VLBW)
hist(IM)
hist(CHD)
hist(LungCan)

#boxplot of rhidata dataset
boxplot(VLBW)
boxplot(IM)
boxplot(CHD)
boxplot(LungCan)

#qplot of rhidata dataset
qplot(VLBW,data = rhidata, geom = "histogram")
qplot(IM,data = rhidata, geom = "histogram")
qplot(CHD,data = rhidata, geom = "histogram")
qplot(LungCan,data = rhidata, geom = "histogram")

mydata = rhidata[,3:6]
round(cor(mydata),1) #correlations between two variables
library (corrplot)
par (mfrow=c(1,1))
corrplot (cor(mydata)) #plot the correlations

#Scree plot to deterine the number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
}   
plot(1:15, wss,type="l", xlab="Number of Clusters",ylab="Within groups sum of squares")
#The location of the elbow in the resulting plot suggests 
#a suitable number of clusters for the kmeans is 4

rhidataK4 <- kmeans(x = mydata, centers = 4)
rhidataK4
#K-means clustering with 4 clusters of sizes 585, 986, 823, 466

#Worcester is in the 3rd clustering vector
#Cluster means:
#      VLBW       IM      CHD  LungCan
#3 5.704739 5.663426 5.217497 5.653706
#In the cluster, Worcester's VLBW, IM and CHD is less than the mean of VLBW, IM and CHD,
#but it has the larger LungCan than the mean of LungCan, 
#which means for VLBW, IM and CHD, Worcester is the same favorable to peers, 
#but more favorable the U.S. Rate than the other counties in US;
#for LungCan, Worcester is the less favorable to peers, but more favorable the U.S. Rate 
#than the other counties in US
#Within cluster sum of squares by cluster:
#[1] 2168.595 3498.668 2983.395 1620.693
#the 3rd cluster has the third smallest sum of squares within the three clusters,
#which means the individual indicators in Worcester's group 
#has differences not too small  

plot(rhidataK4, data = rhidata)
 