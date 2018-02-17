library(ISLR)
set.seed(1)
train=sample (392,196)

lm.fit=lm(mpg~horsepower ,data=Auto,subset=train)

mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)

library(boot)
glm.fit=glm(mpg∼horsepower ,data=Auto)
cv.err=cv.glm(Auto ,glm.fit)
cv.err$delta

#LOOCV test
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg∼poly(horsepower ,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

#10-fold CV test
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg ~ poly(horsepower ,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

#bootstrap
alpha.fn=function (data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio ,1:100)
set.seed(1)
alpha.fn(Portfolio ,sample (100,100, replace=T))

boot(Portfolio, alpha.fn, R=1000)


#bootstrap for linear regression coefficients
boot.fn=function (data ,index) return(coef(lm(mpg ~ horsepower ,data=data,subset=index)))
boot.fn(Auto ,1:392)

set.seed(1)
boot.fn(Auto ,sample (392,392, replace=T))
boot(Auto ,boot.fn, 1000)

boot.fn=function (data ,index) coefficients(lm(mpg~horsepower +I(horsepower ^2),data=data ,                   subset=index))
set.seed(1)
boot(Auto ,boot.fn,1000)
