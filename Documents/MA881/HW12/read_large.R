library(ff)
library(ffbase)
library(tabplot)

setwd("/Users/jiayuan/Documents/MA881/hw12")
#https://data.cityofboston.gov/City-Services/311-Service-Requests/awu8-dc52
requests <- read.csv.ffdf(file="311__Service_Requests-2.csv", header=TRUE, VERBOSE=TRUE, 
                          first.rows=10000, next.rows=50000, colClasses=NA)

requests <- read.csv.ffdf(file="https://dl.dropboxusercontent.com/u/382061/311__Service_Requests.csv", header=TRUE, VERBOSE=TRUE, 
                          first.rows=10000, next.rows=50000, colClasses=NA)

dim(requests)
names(requests)
head(requests)
str(requests[1:10,])

ontime <- requests$OnTime_Status
length(ontime)

colnames(requests)
tableplot(requests,select = c(11,19),nBins=10)

p <- requests$precinct
str(p)
length(p)
head(p)

levels.ff(ontime)
sum(ontime[1:722778]=="ONTIME")

ffsave(requests, file="req")

reqs <- ffload("req")



# Start the clock!
ptm <- proc.time()
sum(ontime[1:722778]=="ONTIME")

# Stop the clock
proc.time() - ptm

system.time(sum(ontime[1:722778]=="ONTIME"))

length(requests$precinct)
any(requests$precinct,NA)

prec <- requests$precinct[!is.na(requests$precinct)]

hist(prec, plot = TRUE)



##################################################################################
str(requests[1:10,])
# $ OnTime_Status: Factor w/ 3 levels "","ONTIME","OVERDUE": 2 2 2 3 2 2 3 3 2 3

ontime <- requests$OnTime_Status
length(ontime)
str(ontime)
length(ontime) #722778
head(ontime)

levels.ff(ontime)
sum(ontime[1:722778]=="ONTIME") #568475
sum(ontime[1:722778]=="OVERDUE") #153713
sum(ontime[1:722778]=="") #590
568475+153713+590 #722778

ffsave(requests, file="req")

reqs <- ffload("req")

# Start the clock!
ptm <- proc.time()
sum(ontime[1:722778]=="ONTIME")

# Stop the clock
proc.time() - ptm

system.time(sum(ontime[1:722778]=="ONTIME"))

# The variable city_council_district is not a factor.
length(requests$city_council_district)
str(requests$city_council_district)
head(requests$city_council_district)
any(requests$city_council_district,NA)

council <- requests$city_council_district[!is.na(requests$city_council_district)]
hist(council, plot = TRUE)

# Start the clock!
ptm <- proc.time()
council <- requests$city_council_district[!is.na(requests$city_council_district)]
hist(council, plot = TRUE)
# Stop the clock
proc.time() - ptm

ffsave(requests, file="req")

reqs <- ffload("req")
