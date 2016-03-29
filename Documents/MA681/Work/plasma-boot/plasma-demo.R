options(digits=2)
no.dis <- read.csv("plasma-no-disease.csv", header=TRUE)
dis <- read.csv("plasma-disease.csv", header = TRUE)
head(no.dis)
head(dis)
nd1 <- no.dis[,1]
d1 <- dis[,1]
nd1.n <- length(nd1)
d1.n <- length(d1)
th.hat <- median(d1)-median(nd1)
B <- 1000
Tboot <- rep(0,B)

for(i in 1:B){
  d1.s <- sample(d1, d1.n, replace=TRUE)
  nd1.s <- sample(nd1, nd1.n, replace=TRUE)
  Tboot[i] <- median(d1.s) - median(nd1.s)
}

se <- sqrt(var(Tboot))


Normal <- c(th.hat - 2*se, th.hat + 2*se)
Percentile <- c(quantile(Tboot,.025),quantile(Tboot,.975))
pivotal <- c((2*th.hat - quantile(Tboot, .975)),(2*th.hat - quantile(Tboot, .025))) 

cat("Method       95% Interval\n")
cat("Normal      (", Normal[1], ",   ",Normal[2], ")\n")
cat("Pivotal     (", pivotal[1], ",     ", pivotal[2], ") \n")
cat("Percentile  (", Percentile[1], ",    ", Percentile[2], ") \n")

