library(ggplot2)
setwd("/Users/jiayuan/Documents/MA881")
data <- read.csv("markets.csv", header=TRUE)
delete<-which(data$Value==" (D)")
data<-data[-c(delete),] 
attach(data)
head(data$Value)
value2 <- gsub(pattern=",", replacement="", data$Value)
head(value2)
value3 <- as.numeric(value2)
head(value3)
class(value3)
hist(value3)
boxplot(value3)

State.ANSI<-data$State.ANSI
corn.val<-value3[Commodity=="CORN"]
rice.val<-value3[Commodity=="RICE"]
corn.state<-State.ANSI[Commodity=="CORN"]
rice.state<-State.ANSI[Commodity=="RICE"]
corn.year<-Year[Commodity=="CORN"]
corn.year<-as.factor(corn.year)
rice.year<-Year[Commodity=="RICE"]
rice.year<-as.factor(rice.year)
corn.com<-Commodity[Commodity=="CORN"]
rice.com<-Commodity[Commodity=="RICE"]

# the counts of different values for corn and rice
par(mfrow=c(1,2))
hist(corn.val, main = "Histogram of CORN")
hist(rice.val, main = "Histogram of RICE")


corn1 <- data.frame(corn.state, corn.val, corn.year)
rice1 <- data.frame(rice.state, rice.val, rice.year)

# different forms of visual description for the values of corn and rice
qplot(corn.val,data = corn1, geom = "histogram")
qplot(corn.val, data = corn1, geom = "density")
qplot(corn.val, data = corn1, geom = "bar")
qplot(rice.val, data = rice1, geom = "histogram")
qplot(rice.val, data = rice1, geom = "density")
qplot(rice.val, data = rice1, geom = "bar")


# the distribution of values of corn and rice in different states
qplot(corn.state, corn.val, data = corn1)
qplot(rice.state, rice.val, data = rice1)
qplot(corn.state, corn.val, data = corn1, geom = c("point", "smooth"),span = 0.2)
qplot(rice.state, rice.val, data = rice1, geom = c("point", "smooth"),span = 0.2)
qplot(corn.state, corn.val, data = corn1, geom = c("point", "smooth"),span = 1)
qplot(rice.state, rice.val, data = rice1, geom = c("point", "smooth"),span = 1)
qplot(corn.state, corn.val, data = corn1, color=corn.year)
qplot(rice.state, rice.val, data = rice1, color=rice.year)
qplot(corn.state, corn.val, data = corn1, color=corn.year, geom = "jitter")
qplot(rice.state, rice.val, data = rice1, color=rice.year,geom = "jitter")
qplot(corn.state, corn.val, data = corn1, color=corn.year, alpha = I(1/10))
qplot(rice.state, rice.val, data = rice1, color=rice.year, alpha = I(1/10))
qplot(corn.state, corn.val, data = corn1, color=corn.year, alpha = I(1/100))
qplot(rice.state, rice.val, data = rice1, color=rice.year, alpha = I(1/100))
qplot(corn.state, corn.val, data = corn1, color=corn.year, alpha = I(1/200))
qplot(rice.state, rice.val, data = rice1, color=rice.year, alpha = I(1/200))
qplot(x=corn.state, y = corn.val, data = corn1, shape=corn.year)
qplot(x=rice.state, y = rice.val, data = rice1, shape=rice.year)

# the counts of values in separate years 
qplot(corn.val, data=corn1, facets=corn.year~.,geom="histogram")
qplot(rice.val, data=rice1, facets=rice.year~., geom="histogram")


p1 <- qplot(corn.val, data=corn1, geom="histogram")
# counts of corn values of different years
p1+facet_wrap(~corn.year)
# counts of corn values of different states
p1+facet_grid(~corn.state)


p2 <- qplot(rice.val, data=rice1, geom="histogram")
# counts of rice values of different years
p2+facet_wrap(~rice.year)
# counts of rice values of different states
p2+facet_grid(~rice.state)





ggplot(data, aes(x = State)) + geom_bar()
ggplot(corn1, aes(x = corn.state)) + geom_bar()
ggplot(rice1, aes(x = rice.state)) + geom_bar()

# use bar chart to display two categorical variables
# now we see the corn and rice counts for each state
with(data, table(Commodity, State))
ggplot(data, aes(x = State, fill = Commodity)) + geom_bar()

# look at the proportion of corn and rice in each state
ggplot(data, aes(x = State, fill = Commodity)) + geom_bar(position = "fill")

# have separate bars for each commodity
ggplot(data, aes(x = State, fill = Commodity)) + geom_bar(position = "dodge")

# transform ANSI into quantitative variable
# histogram of count of observations in each state
as.numeric(as.character(data$State.ANSI))
ggplot(data, aes(x = State.ANSI)) + geom_histogram(binwidth=1)

# present the histogram in a way that makes the total area equals to 1
ggplot(data, aes(x = State.ANSI)) + geom_histogram(binwidth=1, aes(y = ..density..))

# use density plot to produce smooth graph
ggplot(data, aes(x = State.ANSI)) + geom_density()

# use box and whisker plot for comparing two distributions
ggplot(data, aes(x = State, y = Value)) + geom_boxplot()
# flip the coordinates so that the Value variable is horizontal
ggplot(data, aes(x = State, y = Value)) + geom_boxplot() + coord_flip()

# first separate the value by states
# then make a separate dotplot for each state
ggplot(data, aes(x = Value)) + geom_dotplot(dotsize = .1) + facet_grid(State~.)

# compare corn and rice values in a sinple density plot
ggplot(data, aes(x = Value, color = Commodity)) + geom_density()

# generate side-by-side boxplot to examine values for each state
ggplot(data, aes(x = State, y = Value)) + geom_boxplot() + coord_flip()

# use scatterplot to look at the CV in each state
as.numeric(as.character(data$CV....))
ggplot(data, aes(x=State, y=CV....)) + geom_point() + ylab("Coefficient of Variation")


