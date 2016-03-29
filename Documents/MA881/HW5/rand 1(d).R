library(ggplot2)

# random walk function
rands <- function(k){
  # check to make sure input is an integer.
   k1 = as.integer(k)
  if(k1 != k)stop('rands() requires an integer value to start.')
  
  # initialize i and x
  i=1
  x=0
  while(x[i] != k){
    if(runif(1)<.5)D = 1
    else D = -1
    i = i+1
    x[i] = x[i-1]+ D
    
  }
  return(data.frame(x))
  }

## try it with 11

ser <- data.frame(rands(11))
t <- 1:length(ser$x)
qplot(t,x, data=ser, geom="line")


## try it with "hello"

ser <-data.frame("hello")

## try it with 3.4

ser <- data.frame(3.4)

