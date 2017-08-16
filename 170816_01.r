install.packages("RcmdrPlugin.IPSUR")
library(RcmdrPlugin.IPSUR)
data("RcmdrTestDrive")
attach(RcmdrTestDrive)

a = RcmdrTestDrive


summary(a)
table.race = table(a$race)
max.sal = max(a$salary)
which.max(a$salary)
a[152,]

male = a[ which(gender == 'Male'), ]
female = a[ which(gender == 'Female'), ]

mean(male$salary)
mean(female$salary)

by(salary, gender, mean)

par(mfrow=c(2,1))
boxplot(male$salary, female$salary)

par(mfrow=c(2,1))
hist(male$salary)
hist(female$salary)
##############################################


install.packages('prob')
library(prob)

tosscoin(4, makespace = TRUE)
rolldie(2)
permsn(5,3)
combn(5,3)

urnsamples(1:5, 3)
urnsamples(1:5, 3, ordered = TRUE)

a=data.frame(rolldie(2, makespace = TRUE))
attach(a)
a[which(a$X1+a$X2 > 8),]

b = data.frame(tosscoin(3, makespace = TRUE))
attach(b)
b[which(b$toss1 == 'H'),]

a=data.frame(rolldie(2, makespace = TRUE))
attach(a)
a[which(a$X1+a$X2 > 8|X1==X2),]


########
subset(rolldie(2, makespace = TRUE), X1+X2>8)
subset(tosscoin(3, makespace = TRUE), toss1 == 'H')
subset(rolldie(2, makespace = TRUE), a$X1+a$X2 > 8|X1==X2)

s1 <- rolldie(3, makespace = TRUE)
marginal(s1, vars = c("X1", "X3"))
