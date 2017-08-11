fit <- lm(mpg~disp, data=mtcars)
fit1 <- unclass(fit)
class(fit) <- "lm"
plot(fit)
detach(mtcars)
fit2 <- lm(hp~disp, data = mtcars)
plot(hp~disp, data = mtcars)
abline(fit2, col ='red')


y = x^2 + 2x + e


set.seed(1)
x = rnorm(100)
x[1]
y = 2 +2*x + rnorm(100)
plot(x,y, main = "plot(x-y)", type = 'p')

x = seq(-2,2, length = 10)
y = x^2
plot(x,y, type = 'b', lty = 2, pch = 5,  main = "y = x^2")
a <- rep(1,25)
a[(1:25)%%3==0] <- 3
plot(x = 1:25, y = rep(0, 25), lty = 1, pch = a)
############
colors()
plot(x,y, type = "b", xlab = 'xx', ylab = 'yy', main='y = x^2', xlim = c(-1,1), ylim = c(-1,1))
plot(mpg~disp, data = mtcars, xlab = 'dis', ylab = 'mpg', main = 'scatter', pch = 20, col = 'darkblue')

pairs(~mpg+disp+drat+wt, data=mtcars, main='Simple sm')
x = rnorm(100)
y = 2 + 2*x +rnorm(100)
plot(x,y,pch = 20, main = 'scatter')
abline(a = 1, b = 2, col ='red')
abline(v = 1, col = 'blue')
abline(h = 1, col = 'green')

plot(x = 1, y= 1, type = 'n', xlim = c(0,10), ylim = c(0,5), xlab = 'time', ylab = '# of visiting')
x = 0:10
set.seed(1)
y = rpois(length(x), lambda = 1)
points(x,y, col = 'blue', type = 's')
points(x,y, col = 'red', type = 'l')

plot(0,0, type = 'n', xlim = c(-2,2), ylim = c(-2,2))
x = c(-2,1,NA,1,0)
y = c(0,-1,NA,-2,1)
lines(x,y, lty = 2)

plot(0,0, type = 'n', xlim = c(1,5), ylim = c(1,2))
x= seq(1,5, by = 1)
abline(v = x, lty = 1:length(x))



par(mai = c(1,1,1,1))
z = sort(rnorm(100))
y1 = 2 + x + rnorm(100)
plot(z, y1, col = "blue", pch = 3)
points (z, y1/2, col = 'red', pch = 19)
legend('bottomright', c("pch3", "pch 19"), col = c('blue', 'red'), pch = c(3,19))

par (mfrow = c(2,2), bg = 'gray50', col = 'white', col.main = 'lightblue', col.axis = 'yellow', col.lab = 'lightgreen')
x = rnorm(100)
y = 2+2*x + rnorm(100)
plot(x,y,main = "plot (x-y)-1", pch = 20)
y = 2 + x + rnorm(100)
plot(x,y/2, main = 'plot (x-y) -2')
y = 2 + x + rnorm(100)
plot(x,y/3, main = 'plot (x-y) -3')
y = 2 + x + rnorm(100)
plot(x,y/4, main = 'plot (x-y) -4')
y = 2 + x + rnorm(100)

################
set.seed(1)
x <- sort(rnorm(100))
y <- 3+x^2 + rnorm(100)
plot(x,y, pch = 20)
fit <- lm(y~x)
fit$coefficients
abline(a = fit$coefficients[1], b = fit$coefficients[2], col='red')
yTrueMean <- 3+ x^2
lines(x, yTrueMean, lty=2, col='black')
#################################
library(FNN)
knnx.index(x, 0, k = 10)
idx <- c(knnx.index(x, 0, k = 10))
idx
yhat <- mean( y[idx] ) 
yhat
plot()

eval.point = 0
plot(x, y, pch = 20)
abline( v = eval.point , col = 'black')
points(x[idx], y[idx], col = 'red', pch = 20)
abline(h = mean(y[idx]), lty = 2, col = 'red')
text(0,1, labels = round(mean(y[idx]), 3))

###########################
eval.n = 100
eval.point = seq(-3,3, length= eval.n)
plot(x, y, pch = 20)
idx.mat<- knnx.index(x, eval.point , k = 8)
yhat = rep(0,eval.n)
for (i in 1:eval.n) yhat[i]<-mean(y[idx.mat[i,]])
lines(eval.point , yhat, type= 'l', lty = 2, col = 'red')
#########################

install.packages('rgl')
library(rgl)
attach(mtcars)
plot3d(wt, disp, mpg, col='red', size =5)


mypal = c('blue', 'red', 'green')
class(mtcars$cyl)
factor(mtcars$cyl)
mypal[factor(mtcars$cyl)]
plot3d(wt, disp, mpg, col= mypal[factor(mtcars$cyl)], size=5)
############################
library(rgl)
z <- 2 * volcano # Exaggerate the relief
x <- 10 * (1:nrow(z)) # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z)) # 10 meter spacing (E to W)
par(bg = 'slategray')
persp(x,y,z, theta = 135, phi = 30, col = 'green3', scale = FALSE, ltheta = -20, shade = 0.75, border = NA, box =FALSE)
persp3d(x,y,z, col = 'green3')
