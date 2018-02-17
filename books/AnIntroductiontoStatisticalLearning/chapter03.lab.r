library(MASS)
library(ISLR)

#fix(Boston)
names(Boston)

lm.fit=lm(medv ~ lstat, data = Boston)
lm.fit
summary(lm.fit)
coef(lm.fit)
confint(lm.fit)

#the expected mean and its 95% interval
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "confidence")
#the expected value and its 95% interval
#this is necessarily wider because the mean is necessarily smaller then 
#the biggest and bigger than the smallest possible values
predict(lm.fit,data.frame(lstat=c(5,10,15)), interval = "prediction")

plot(Boston$lstat,Boston$medv)
abline(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals (lm.fit))
plot(predict(lm.fit), rstudent (lm.fit))

plot(hatvalues (lm.fit))
which.max(hatvalues (lm.fit))

lm.fit=lm(medv ~lstat+age ,data=Boston)
summary(lm.fit)

lm.fit=lm(medv ~.,data=Boston)
summary(lm.fit)

library(car)
vif(lm.fit)

summary(lm(medv ~ lstat*age,data=Boston))

lm.fit2=lm(medv ~ lstat+I(lstat^2), data = Boston)
summary(lm.fit2)

lm.fit=lm(medv~lstat, data = Boston)

#The null hypothesis is that the two models
#fit the data equally well;
#and the alternative hypothesis is that the full
#model is superior.
#Here the F-statistic is 135 and the associated p-value is
#virtually zero. This provides very clear evidence
#that the model containing
#the predictors lstat and lstat 2 is far superior
#to the model that only
#contains the predictor lstat .
anova(lm.fit ,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5=lm(medv ~ poly(lstat ,5), data = Boston)
summary(lm.fit5)
