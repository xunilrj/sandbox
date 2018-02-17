library(ISLR)
Hitters=na.omit(Hitters)

library(leaps)
regfit.full=regsubsets (Salary~.,Hitters)
regfit.full=regsubsets (Salary~.,data=Hitters ,nvmax=19)
reg.summary=summary(regfit.full)
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",       type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ",       ylab="Adjusted RSq",type="l")
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

#foward and backward stepwise selection
regfit.fwd=regsubsets (Salary~.,data=Hitters ,nvmax=19, method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets (Salary~.,data=Hitters ,nvmax=19, method="backward")
summary(regfit.bwd)


predict.regsubsets =function (object ,newdata ,id,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames =list(NULL , paste(1:19)))
for(j in 1:k){
  best.fit=regsubsets (Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit ,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors ,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors ,type="b")

#Why use the full dataset after the CV?
#https://stats.stackexchange.com/questions/11602/training-with-the-full-dataset-after-cross-validation
reg.best=regsubsets (Salary~.,data=Hitters , nvmax=19)
coef(reg.best ,11)

#RIDGE AND LASSO REGRESSION 

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

library(glmnet)

#ridge
grid=10^seq(10,-2, length =100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

#after findingnthe best RIDGE lanbda, we refit the model using
#the whole dataset

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

#LASSO
lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test ,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef


library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters ,scale=TRUE, validation ="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")

set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters ,subset=train ,scale=TRUE,validation ="CV")
validationplot(pcr.fit,val.type="MSEP")

pcr.pred=predict(pcr.fit ,x[test ,],ncomp=7)
mean((pcr.pred-y.test)^2)

#Finally, we fit PCR on the full data set, 
#using M = 7, the number of
#components identified by cross-validation.
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)


#PLS
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters ,subset=train ,scale=TRUE ,validation ="CV")
summary(pls.fit)
pls.pred=predict(pls.fit ,x[test ,],ncomp=2)
mean((pls.pred-y.test)^2)

#Finally, we perform PLS using the full data set, using M = 2, the number
#of components identified by cross-validation.
pls.fit=plsr(Salary~., data=Hitters ,scale=TRUE,ncomp=2)
summary(pls.fit)
