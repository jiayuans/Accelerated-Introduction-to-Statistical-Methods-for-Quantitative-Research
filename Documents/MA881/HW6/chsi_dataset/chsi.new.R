setwd("/Users/jiayuan/Documents/MA881/HW6/chsi_dataset")
library(ggplot2) 
health <- read.csv("RELATIVEHEALTHIMPORTANCE.csv",header=T)
names(health) <- c('SC', 'CC', 'CN', 'SN', 'SA', 'SIN', 'LBW', 'VLBW', 'Premature', 
                   'U18', 'O40', 'Unmarried','LateCare', 'IM', 'IMWNH', 'IMBNH', 
                   'IMH', 'IMN', 'IMPN', 'BrstCan', 'ColCan', 'CHD', 'Homicide', 
                   'LungCan', 'MVA', 'Stroke', 'Suicide', 'Injury')
attach(health)
Worcester.health <- health[which(CN=="Worcester"),][2,]
Worcester.health
num <- 0
for (i in 1:8){
  num[i] <- length(which(Worcester.health==i))
}
num
#Relative health indicator is 5: 11 measures, 
#Represent ''Favorable to peers and favorable the U.S. Rate 
#Relative health indicator is 6: 1 measures, 
#Represent 'Favorable to peers and unfavorable the U.S. Rate 
#Relative health indicator is 7: 6 measures, 
#Represent 'Unfavorable to peers and favorable the U.S. Rate
#Relative health indicator is 8: 4 measures, 
#Represent 'Unfavorable to peers and unfavorable the U.S. Rate 

#measures of interest:
#SC, CC
#VLBW, IM, CHD, LungCan
rhi <- cbind(SC, CC, VLBW, IM, CHD, LungCan)
head(rhi)
#drop the data of the two situations:  no data available(-2) and no report(-1)
delete=which(VLBW=="-1" | VLBW=="-2" | IM=="-1" | IM=="-2" | CHD=="-1" 
             | CHD=="-2" | LungCan=="-1" | LungCan=="-2")
rhi=rhi[-c(delete),]
#new data frame of the six measures after droping the useless data
rhi=as.data.frame(rhi)
attach(rhi)

#compare the four measures VLBW, IM, CHD, LungCan for differnt counties 
#within MA state
MA <- rhi[which(SC=="25"),] #MA has the state identifier of 25 in US
MA 
nrow(MA) #14 counties in MA
Worcester <- MA[which(MA$CC=="27"),] #Worcester has the county identifier of 27 in MA
Worcester
#    SC CC VLBW IM CHD LungCan
#1150 25 27    5  5   5       7
#VLBW: 5, representing 'Favorable to peers and favorable the U.S. Rate'
#IM: 5, representing 'Favorable to peers and favorable the U.S. Rate'
#CHD: 5, representing 'Favorable to peers and favorable the U.S. Rate'
#LungCan: 7, representing 'Unfavorable to peers and favorable the U.S. Rate'

MAdata = MA[,3:6]
MA.VLBW <- MAdata$VLBW
MA.IM <- MAdata$IM
MA.CHD <- MAdata$CHD
MA.LungCan <- MAdata$LungCan

mysummary <- function (x) {
  options (digits=4)
  matrix (c( mean(x), sd(x), length(x), min(x), 
             quantile(x,.25), median(x),
             quantile(x,.75), max(x), sum(is.na(x))), ncol=9,
          dimnames = list (NULL, c( "Mean", "SD", "n", "Min", 
                                    "1st Qu", "Median", "3rd Qu", "Max", "NAs")))}

#summaries of MAdata data frame
summary (MAdata)
mysummary(MA.VLBW)
mysummary(MA.IM)
mysummary(MA.CHD)
mysummary(MA.LungCan)

#stem-plot of MAdata data frame
stem(MA.VLBW) 
stem(MA.IM) 
stem(MA.CHD) 
stem(MA.LungCan)

#plot of MAdata data frame
par (mfrow=c(1,2))
plot(MA.VLBW)
plot(MA.IM)
plot(MA.CHD)
plot(MA.LungCan)

#histgram of MAdata data frame
hist(MA.VLBW)
hist(MA.IM)
hist(MA.CHD)
hist(MA.LungCan)

#boxplot of MAdata data frame
boxplot(MA.VLBW)
boxplot(MA.IM)
boxplot(MA.CHD)
boxplot(MA.LungCan)

#qplot of MAdata data frame
qplot(MA.VLBW,data = MAdata, geom = "histogram")
qplot(MA.IM,data = MAdata, geom = "histogram")
qplot(MA.CHD,data = MAdata, geom = "histogram")
qplot(MA.LungCan,data = MAdata, geom = "histogram")

#use The Elbow Method to look at the percentage of variance 
#explained as a function of the number of clusters
MAwss <- (nrow(MAdata)-1)*sum(apply(MAdata,2,var))
for (i in 2:8) {
  MAwss[i] <- sum(kmeans(MAdata,centers=i)$withinss)
}   
#plot to deterine the number of clusters
plot(1:8, MAwss,type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#The location of the elbow in the resulting plot suggests 
#a suitable number of clusters for the kmeans is 3
MAK3 <- kmeans(x = MAdata, centers = 3)
MAK3

#plot the 3 groups of MAdata data frame
require(useful)
par (mfrow=c(1,1))
plot(MAK3, data = MAdata)

#compare the four measures VLBW, IM, CHD, LungCan for differnt counties 
#within the U.S.
rhidata = rhi[,3:6]
#summaries of rhidata dataset
summary (rhidata)
mysummary(VLBW)
mysummary(IM)
mysummary(CHD)
mysummary(LungCan)

#plot of rhidata dataset
par (mfrow=c(1,2))
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

#use The Elbow Method to look at the percentage of variance 
#explained as a function of the number of clusters
wss <- (nrow(rhidata)-1)*sum(apply(rhidata,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(rhidata,centers=i)$withinss)
}   
#plot to deterine the number of clusters
plot(1:15, wss,type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#The location of the elbow in the resulting plot suggests 
#a suitable number of clusters for the kmeans is 4
rhidataK4 <- kmeans(x = rhidata, centers = 4)
rhidataK4

#plot the 4 groups of rhidata data frame
par (mfrow=c(1,1))
plot(rhidataK4, data = rhidata)
