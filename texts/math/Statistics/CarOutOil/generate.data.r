days = seq(from = 1, to = 260)
type = c(1,2)
homeWork = rnorm(260, mean=10, sd=1.5)
workHome = rnorm(260, mean=10, sd=2.0)

consuption = data.frame(days, type, consumption = c(homeWork, workHome))
write.csv(file="consumption.csv", x=consumption)

consumption = read.csv("consumption.csv")

homeWork = consumption[comsumption$type == 1,]
workHome = consumption[comsumption$type == 2,]

png(filename="homeWork.plot.png")
plot(homeWork$day, homeWork$consumption)
dev.off()

png(filename="homeWork.hist.png")
hist(homeWork$consumption)
dev.off()

png(filename="workHome.plot.png")
plot(workHome$day, workHome$consuption)
dev.off()

png(filename="workHome.hist.png")
hist(workHome$consuption)
dev.off()

normal.lik<-function(mu,sigma,y){
  sigma <- max(sigma, 0.0001)
  n<-length(y)
  logl <- sum(log(dnorm(y, mean = mu, sd = sigma)))/n
  return(-logl)
}
normal.lik.muvar <- function (theta,y){
  normal.lik(theta[1],theta[2],y)
}
v.normal.lik <- function (mu, sigma, y){
  mapply(function(m,s) normal.lik(m,s,y), mu, sigma)
}

library(RColorBrewer)
plotMLE <- function(data, x, y, steps = 20, levels = 10000) {
  x = seq(x[1], x[2], length= 20)
  y = seq(y[1], y[2], length= 20)
  f = function(x, y) {
    v.normal.lik(x,y,data)
  }
  z = outer(x, y, f)
  z[is.na(z)] = 0
  contour(x,y,z,col=rev(brewer.pal(11, "RdYlBu")), nlevels = levels,
    xlab="mean", ylab="standard deviation")
}

png(filename="homeWork.likelihood.png")
  plotMLE(homeWork$consumption, x=c(5,15),y=c(0,5), levels = 2000)
dev.off()


optim(c(12,4),normal.lik.muvar,y=homeWork$consumption,method="BFGS") 



png(filename="homeWork.likelihood.withlines.png")
  plotMLE(homeWork$consumption, x=c(5,15),y=c(0,5), levels = 2000)
  abline(h=1.70105, lty=2)
  abline(v=10.01983, lty=2)
dev.off()

plotGaussian <- function(start = -1, end = 1, u, sigma, scale, color = "black"){
  x <- seq(start, end, length=100)
  hx <- dnorm(x, mean = u, sd = sigma)*scale
  lines(x, hx, type="l", lty=2, xlab="x value", ylab="Density", main="Gaussian", col = color)
}

png(filename="homeWork.hist.model.png")
  plot.new()
  hist(homeWork$consumption)
  plotGaussian(5, 15, 10, 1.7, 250, "red")
dev.off()
