

reg = 50 + 20x1 + 0.07x2 +  35x3 + 0.01x4 - 10x5
             gpa     iq      gen     gpa&iq    gpa&gen
             
man = 50 + 80 + 7 + 0 + 4.4 
female = 50 + 80 + 7 + 35 + 4.4 - 10*4*1            
            
f = 50 + 20*4 + 0.07*110 + 35*1 + 0.01*(110*4) - 10*(4*1)
x1 = 110
gpa = 4
207.1

gpa&iq 의 교호작용에 따른 유의성 수준을 알기 어렵다.

######################################

set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)


cor(x1, x2)
plot(x1, x2)
abline(lm(x2~x1), col='red')

lm.fit=lm(y~x1+x2)
summary(lm.fit)
lm.fit3=lm(y~x2)
summary(lm.fit3)
lm.fit4=lm(y~x1*x2)
summary(lm.fit4)

########################################################################################################

library(ISLR)
attach(Smarket)
str(Smarket)
head(Smarket)
cor(Smarket[,-9])

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = binomial)
summary(glm.fit)

glm.prob = predict(glm.fit, type = 'response')

glm.pred=rep("down", 1250)
glm.pred[glm.prob >0.5] = "Up"
table(glm.pred,Smarket$Direction)
(211+440)/1250

  library(MASS)-
data(Smarket)
train=(Year<2005)
Smarket.2005=Smarket[!train,]
str(Smarket.2005)
Direction.2005<-Direction[!train]

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,subset=train,family="binomial")
glm.prob=predict(glm.fit,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.prob>0.5]="Up"
table(glm.pred,Direction.2005)

glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,subset=train,family="binomial")
glm.prob=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.prob>0.5]="Up"
table(glm.pred,Direction.2005)

predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit

lda.pred=predict(lda.fit,Smarket.2005)
table(lda.pred$class,Direction.2005)

head(lda.pred$posterior)
head(lda.pred$x)
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.pred=predict(qda.fit,Smarket.2005)
table(qda.pred$class,Direction.2005)


library(class)
train.X<-Smarket[train,2:3]
test.X<-Smarket[!train,2:3]
train.Direction<-Direction[train]

knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)