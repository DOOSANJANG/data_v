german = read.csv('german_numeric.csv', header = T)

names(german)
dim(german)
summary(german)

library(class)
dim(german)


train = c(1:600)
test = c(601:1000)
german[,16] <- german[,16]-1
Response <- rep(0,1000)

for (i in c(1:1000))
{
  if (german[i,16]==0){Response[i]='Good'}
  else {Response[i]='Bad'}
}

attach(german)

train.x = german[train,-16]
test.x = german[test,-16]
train.result = Result[train]
test.result = Result[test]

set.seed(1)
glm.fit<-glm(Result~.,data=german,family=binomial)

summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10]
glm.pred=rep("Good",1000)
glm.pred[glm.probs>0.5]='Bad'
table(glm.pred,Response)
(137+629)/1000
mean(glm.pred==Response)


library(MASS)
lda.fit = lda(Result ~.,data = german, subset = train)
lda.pred = predict(lda.fit, test.x)
summary(lda.pred)
lda.class=lda.pred$class
names(lda.pred)
summary(lda.class)
table(lda.class,test.result)
mean(lda.class==test.result)
mean(lda.class !=test.result)

qda.fit = qda(Result ~ ., data = german, subset = train)
qda.pred = predict(qda.fit, test.x)
qda.class=qda.pred$class
mean(qda.pred$class != test.result)
mean(qda.class==test.result)
table(qda.class,test.result)
