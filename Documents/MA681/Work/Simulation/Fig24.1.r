### Figure 24.1  
### 
library(ggplot2)
x = 8
y = 6
n = 10
m = 10

N = 10000
p1 = rbeta(N,x+1,n-x+1)
p2 = rbeta(N,y+1,m-y+1)

diff=p2-p1

diff.mean <- mean(diff)

#posterior 95% Interval
lower <- quantile(diff, .025)
upper <- quantile(diff, .975)

qplot(diff, geom="histogram", binwidth=.1, 
      main = "Posterior Distribution",
      xlab = "p"
)








