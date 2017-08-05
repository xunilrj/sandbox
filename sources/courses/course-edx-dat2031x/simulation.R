rlookup <- function(num, data){
  unif <- runif(num)
  f <- function(v) {subset(data, v >= start & v < end)$value}
  sapply(unif, f)
}

profits <- function(num){
  start <- c(.Machine$double.xmin, 0.3, 0.6)
  end <- c(0.3, 0.6, .Machine$double.xmax) # we must finish after 1.0 because runif can generate 1.0
  value <- c(5,3.5, 4)
  data <- data.frame(start, end, value)
  rlookup(num, data)
}

tips <- function(num){
  start <- c(.Machine$double.xmin, 0.5, 0.7, 0.9)
  end <- c(0.5, 0.7, 0.9, .Machine$double.xmax) # we must finish after 1.0 because runif can generate 1.0
  value <- c(0, 0.25, 1, 2)
  data <- data.frame(start, end, value)
  rlookup(num, data)
}

generateHist <- function(data, bins){
  maxm <- max(data)
  minm <- min(data)
  bw <- (maxm - minm)/bins
  breaks <- seq(minm - bw/2, maxm + bw/2, by = bw)
  h <- hist(data, breaks = breaks, freq = FALSE, plot = FALSE)
  h
}

num <- 100000
mean <- 600
sd <- 30
bins <- 120

profit <- profits(num)
tips <- tips(num)

clientArrival <- rnorm(num, mean, sd)
net <- (clientArrival*profit)+(clientArrival*tips)
mean(net)
sd(net)
var(net)
asNormal <- generateHist(net, bins)

clientArrival <- rpois(num, mean)
net <- (clientArrival*profit)+(clientArrival*tips)
mean(net)
sd(net)
var(net)
asPois <- generateHist(net, bins)

plot(asNormal, col=rgb(0,0,1,1/4), main = "", xlab = "Profit", ylab = "Probability")
plot(asPois, col=rgb(1,0,0,1/4), add = T)
legend('topright', c("Normal","Poisson"), lty=1, col=c('blue', 'red'), bty='n', cex=.75)
title(main = "Net Profit")

