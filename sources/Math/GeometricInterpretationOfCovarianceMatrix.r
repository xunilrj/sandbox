library(scales)
library(MASS)

generateData <- function (u, sdx, sdy){
  size <- 1000
  x0 <- rnorm(mean = u, sd = sdx, n = size)
  x1 <- rnorm(mean = u, sd = sdy, n = size)
  X <- matrix(x0, ncol = 1)
  cbind(X, matrix(x1, ncol = 1))
}

plotMatrix <- function(X){
  xdf <- data.frame(X)
  plot(xdf, ylim=c(-10, 10), asp=1, col = alpha("black",0.5))
}

plotEigenvectors <- function(X){
  C <- covarianceMatrix(X)
  eigenC <- eigen(C, symmetric = TRUE)
  dimensions <- eigenC$vectors %*% diag(sqrt(eigenC$values))
  arrows(0,0,dimensions[1,1],dimensions[2,1], lwd = 4, col = "red")
  arrows(0,0,dimensions[1,2],dimensions[2,2], lwd = 4, col = "blue")
}

rotationMatrix <- function(angle){
  matrix(c(cos(angle),sin(angle),-sin(angle),cos(angle)), nrow = 2, ncol= 2)
}

covarianceMatrix <- function(X){
  mu <- matrix(colMeans(X), nrow = 1)
  t(X)%*%X * (1/(nrow(X)-1)) - (t(mu)%*%mu)
}

#Theory

C <- diag(1,2,2)
eigenC <- eigen(C)
dimensions <- eigenC$vectors %*% diag(eigenC$values) 

#Simulatiom - white data u = 0 and sd = 1

X <- generateData(0,1,1)
plotMatrix(X)
plotEigenvectors(X)

#gaussian with u = 0 and sd = (4,1)

X <- generateData(0, 4, 1)
plotMatrix(X)
plotEigenvectors(X)

#White data u = 0 and sd = 1 rotate in 45 degrees CCW

X <- generateData(0, 4, 1)
rot45 <- rotationMatrix(-pi/4)
X <- X %*% rot45
C <- covarianceMatrix(X)
eigenC <- eigen(C)
plotMatrix(X)
plotEigenvectors(X)

X <- generateData(0,1,1)
R <- rotationMatrix(-pi/4)
S <- matrix(c(4,0,0,1), nrow=2, ncol=2)
T <- R%*%S
#R is a orthonormal matrix so t(R) = ginv(R)
C <- T%*%t(T)
plotMatrix(X)
plotMatrix(X%*%S%*%R)
eigenVectors <- t(R)%*%S
arrows(0,0,eigenVectors[1,1],eigenVectors[2,1], lwd = 4, col = "red")
arrows(0,0,eigenVectors[1,2],eigenVectors[2,2], lwd = 4, col = "blue")

# These matrices should be as near as zero as possible
eigenC$vectors - t(R)
diag(sqrt(eigenC$values), 2, 2) - S
