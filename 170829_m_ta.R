A=matrix(c(1,2,0,0,3,0,2,-4,2),nrow=3,byrow=TRUE)
B=eigen(A)
value=diag(B$values)
vector=B$vectors


A<-matrix(c(1,0,0,0,2,0,0,3,0,0,0,0,0,0,0,0,2,0,0,0),nrow=4,byrow=TRUE)
res<-svd(A)
res

res$u%*%diag(res$d)%*%t(res$v)

t(A)


library(ISLR)

attach(Hitters)

names(Hitters)
head(Hitters)
##############################
library(MASS)
attach(Auto)
head(Auto)


aaa = c(1,2,3,4)
poly(aaa, 3)

Auto[-trainx,]

set.seed(1)
trainx<-sample(392,392*0.5)
lm.fit <- lm(mpg~horsepower, data=Auto, subset = trainx)
predict(lm.fit, Auto[-trainx,])
mean((mpg[-trainx]-predict(lm.fit, Auto[-trainx,]))^2)

lm.fit2 <- lm(mpg~poly(horsepower,2), subset = trainx)
mean((mpg[-trainx]-predict(lm.fit2, Auto[-trainx,]))^2)

lm.fit3 <- lm(mpg~poly(horsepower,3), subset = trainx)
mean((mpg[-trainx]-predict(lm.fit3, Auto[-trainx,]))^2)

##############################
library(boot)

glm.fit <- glm(mpg~horsepower, data=Auto)
coef(glm.fit)
cv.err=cv.glm(Auto, glm.fit)
cv.err$call
cv.err$delta


glm.fit2=glm(mpg~poly(horsepower, 2))
cv.err2=cv.glm(Auto,glm.fit2)

glm.fit3=glm(mpg~poly(horsepower, 3))
cv.err3=cv.glm(Auto,glm.fit3)
cv.err3$delta

################################

alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(X)-cov(X,Y))/(var((X)+var(Y)-2*cov(X,Y))))
}
alpha.fn(Portfolio, 1:100)




##################################
boot.fn=function(data,index)
  {return(coef(lm(mpg~horsepower,data=data,subset=index)))}

boot(Auto,boot.fn,5000)
summary(lm(mpg~horsepower,data=Auto))$coef
#######################################################
library(ISLR)
names(Hitters)
dim(Hitters)
str(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
library(leaps)
regfit.full=regsubsets(Salary~., Hitters)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq


regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)


#################
library(glmnet)
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) #lambda <- 허용 벡터 크기(람다, ex. b1 + b2 < c(람다)) / alpha -> 0에서 1값을 가짐 (라그랑쥬 승수법), 0(능선), 1(라소), 0~1사이(혼합)

predict(ridge.mod, s=5, type="coefficients") # s<-람다 No.

