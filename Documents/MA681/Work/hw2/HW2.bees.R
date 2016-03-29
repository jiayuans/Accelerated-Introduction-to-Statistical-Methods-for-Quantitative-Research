library(ggplot2)
bees <- read.csv("/users/jiayuan/Documents/MA681/bees.csv",header = FALSE)
bees <- data.frame(bees)
bees1 <- sort(bees$V1, decreasing=FALSE)
ecdf(bees1)
plot(ecdf(bees1))
qqnorm(bees1)
hist(bees1)
qplot(data=bees, x=V1, stat = "ecdf", geom = "step")
ks.test(bees1,y="pnorm")
bees1



