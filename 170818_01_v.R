if (!require(plotrix)) install.packages('plotrix')
if (!require(vioplot)) install.packages('vioplot')
if (!require(vcd)) install.packages('vcd')

data(mtcars)
attach(mtcars)

cyl.name = c('4cyl', '6cyl', '8cyl')
a=table(mtcars$cyl)
barplot(a, main = 'cyl chart', xlab = 'cylinder', ylab = 'No', names.arg = cyl.name)

cyl.name2 = paste0( cyl.name, "(", a, "%)")

paste ("a","b", sep = '_')

pie(a, labels = cyl.name2, col = rainbow(length(a)), main = 'pie chart')

library(plotrix)

pie3D(a, labels = cyl.name2, explode = 0.1, main = '3d')

fan.plot(a, labels = cyl.name2, main = 'Fan plot')

library(vcd)
head(Arthritis)

my.table <- xtabs( ~ Treatment + Improved, data = Arthritis)

barplot(my.table, xlab = 'Improved', ylab='Frequency', legend.text = TRUE, col = c('green', 'red'))

barplot(t(my.table), xlab = 'Improved', ylab='Frequency', legend.text = TRUE, col = c('green', 'red', 'orange'))


t(my.table)
my.table


tmp = c('buckled', 'unbuckled')
belt <- matrix(c(58,2,8,16), ncol = 2, dimnames = list(parent = tmp, child = tmp))
spine(belt, main = 'spine chart', gp = gpar(fill = c('green', 'red')))

x = faithful$waiting
hist(x, nclass = 8)

hist(x, breaks= seq(min(x), max(x), length = 10), probability = T)
lines(density(x), col = 'blue', lwd = 1)

library(vioplot)
x = rpois(1000, lambda = 3)
vioplot(x, col = 'yellow')


attach(mtcars)
boxplot(mpg~cyl, data = mtcars, names = c('4c', '6c', '8c'), main = 'test')

par(mfrow = c(3,1))
hist(mpg[cyl==4], xlab = 'MPG', main = 'MPG dist by cyl', xlim = c(5,40), ylim = c(0,10), col = 'red')
hist(mpg[cyl==6], xlab = 'MPG', main = 'MPG dist by cyl', xlim = c(5,40), ylim = c(0,10), col = 'blue')
hist(mpg[cyl==8], xlab = 'MPG', main = 'MPG dist by cyl', xlim = c(5,40), ylim = c(0,10), col = 'green')



mycol = colors()
plot(1:80, y=rep(1,80), col = mycol[1:80], cex = 2, pch = 2, ylim = c(0,1))

mycol <- heat.colors(4, alpha = 1)
plot(1:4, rep(0,4), col = mycol, pch=20)
mycol2 <- topo.colors(100, alpha = 1)
plot(1:100, rep(0, 100), col = mycol2, pch = 20)


x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col = my_col, axes = FALSE)
contour(x, y, volcano, levels = seq(90, 200, by = 5),
        add = TRUE, col = 'white')


library(colorspace)
pal = choose_palette()

my_col = pal(7)
my_col
