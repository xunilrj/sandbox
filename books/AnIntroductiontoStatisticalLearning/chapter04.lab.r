library(ISLR)
names(Smarket)
dim(Smarket)

cor(Smarket[,-9])

plot(Smarket$Volume)

glm.fit=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)

glm.probs=predict(glm.fit,type="response")
glm.pred=rep("Down",1250)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Smarket$Direction)
#in training error rate
mean(glm.pred==Smarket$Direction )

train=(Smarket$Year <2005)
Smarket.2005 = Smarket[!train ,]
dim(Smarket.2005)
Direction.2005= Smarket$Direction[!train]

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)

mean(glm.pred==Direction.2005)


library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket ,subset=train)
lda.fit
plot(lda.fit)

lda.pred=predict(lda.fit , Smarket.2005)
lda.class=lda.pred$class
table(lda.class, Direction.2005)

qda.fit=qda(Direction ~ Lag1+Lag2,data=Smarket ,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

library(class)
train.X=cbind(Smarket$Lag1, Smarket$Lag2)[train ,]
test.X=cbind(Smarket$Lag1,Smarket$Lag2)[!train ,]
train.Direction = Smarket$Direction [train]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction, k=1)
table(knn.pred,Direction.2005)

knn.pred=knn(train.X,test.X,train.Direction, k=3)
table(knn.pred,Direction.2005)

dim(Caravan)
standardized.X=scale(Caravan [,-86])
var(Caravan [,1])
var(standardized.X[,1])
test=1:1000
train.X=standardized.X[-test ,]
test.X=standardized.X[test ,]
train.Y=Caravan$Purchase [-test]
test.Y=Caravan$Purchase [test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")

table(knn.pred,test.Y)


glm.fit=glm(Purchase~.,data=Caravan ,family=binomial, subset=-test)
glm.probs=predict(glm.fit,Caravan[test ,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs >.5]="Yes"
table(glm.pred,test.Y)

glm.pred=rep("No",1000)
glm.pred[glm.probs >.25]="Yes"
table(glm.pred,test.Y)
