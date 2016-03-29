library(ggplot2)
options(digits=2)
nerve <- read.csv("nerve.csv",header=FALSE)
head(nerve)
plot(ecdf(nerve$V1))
nrv <- nerve[,1]
nrv.n <- length(nrv)
class(nrv)
B <- 1000
Tboot <- rep(0,B)
for(i in 1:B){
  nrv.s <- sample(nrv, nrv.n, replace=TRUE)
  Tboot[i] <- median(nrv.s)
}
se <- sqrt(var(Tboot))
se
