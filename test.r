# R code 

## Rstat

if (!require(plotrix)) install.packages("plotrix")
if (!require(vioplot)) install.packages("vioplot")
if (!require(vcd)) install.packages("vcd")

counts = table(state.region)
counts
barplot(counts, main = "simple bar chart", 
        xlab = "region", ylab = "freq")

freq.cyl =table(mtcars$cyl)
barplot(freq.cyl, main = "simple bar chart", col ="orange")

cyl.name = c("4 cyl", "6 cyl", "8 cyl")
barplot(freq.cyl, main = "simple bar chart", col ="orange",
        names.arg = cyl.name)

cyl.name2 = paste0( cyl.name, "(", freq.cyl, "%)")
pie(freq.cyl, labels = cyl.name2, 
    col = rainbow(length(freq.cyl)), main = "pie chart")


library(plotrix)
pie3D(freq.cyl, labels = cyl.name2, explode = 0.1, main = "3d pie plot")

fan.plot(freq.cyl, labels = cyl.name2, main = "Fan plot")

## Frequency table
library(vcd)
head(Arthritis, n = 3)

## Frequency table
my.table <- xtabs( ~ Treatment + Improved, data = Arthritis)
my.table

## barplot
barplot( my.table,
         xlab = "Improved", ylab = "Frequency", legend.text = TRUE,
         col = c("green", "red"))

## barplot
barplot( t(my.table),
         xlab = "Improved", ylab = "Frequency", legend.text = TRUE,
         col = c("green", "red", "orange"))
t(my.table)

barplot( t(my.table),
         xlab = "Improved", ylab = "Frequency", legend.text = TRUE,
         col = c("green", "red", "orange"))

tmp = c("buckled", "unbuckled")
belt <- matrix( c(58, 2, 8, 16), ncol = 2, 
                dimnames = list(parent = tmp, child = tmp))
belt

library(vcd)
spine(belt, main="spine plot for child seat-belt usage",
      gp = gpar(fill = c("green", "red")))

x = rnorm(100)
boxplot(x, main = "boxplot", col ='lightblue')
## histogram
x = faithful$waiting
hist(faithful$waiting, nclass = 8)

x = faithful$waiting
hist(faithful$waiting, breaks = seq(min(x), max(x), length = 10),
     probability = T)

x = faithful$waiting
hist(faithful$waiting, nclass = 10, probability = T)
lines(density(x), col = "red", lwd = 2)

library(vioplot)
x = rpois(1000, lambda = 3)
vioplot(x, col = "lightblue")


## violin plot
library(vioplot)
x = rpois(1000, lambda = 3)
vioplot(x, col = "lightblue", names = 'variable')

# visualization for mutivariate variables

## multiple boxplot

attach(mtcars)
boxplot(mpg~cyl, data = mtcars, names = c('4 cyl','6 cyl', '8 cyl'),
        main = "MPG dist by cylinder")

## multiple histogram
hist(mpg[cyl==4], xlab="MPG", main = "MPG dist by cylinder",
     xlim = c(5, 40), ylim = c(0,10), col = 'lightblue',
     nclass = trunc(sqrt(length(mpg[cyl==4]))))
hist(mpg[cyl==6], xlab="MPG", main = "MPG dist by cylinder",
     xlim = c(5, 40), ylim = c(0,10), col = 'orange',
     nclass = trunc(sqrt(length(mpg[cyl==6]))), add= TRUE)
hist(mpg[cyl==8], xlab="MPG", main = "MPG dist by cylinder",
     xlim = c(5, 40), ylim = c(0,10), col = 'red',
     nclass = trunc(sqrt(length(mpg[cyl==8]))), add= TRUE)


## multiple histogram

par(mfrow = c(3,1))
hist(mpg[cyl==4], xlab="MPG", main = "MPG dist by cylinder",
     xlim = c(5, 40), ylim = c(0,10), col = 'lightblue',
     nclass = trunc(sqrt(length(mpg[cyl==4]))))
hist(mpg[cyl==6], xlab="MPG", main = "MPG dist by cylinder",
     xlim = c(5, 40), ylim = c(0,10), col = 'orange',
     nclass = trunc(sqrt(length(mpg[cyl==6]))))
hist(mpg[cyl==8], xlab="MPG", main = "MPG dist by cylinder",
     xlim = c(5, 40), ylim = c(0,10), col = 'red',
     nclass = trunc(sqrt(length(mpg[cyl==8]))))


plot(density(mpg[cyl==4]), xlab="MPG", main = "MPG dist by cylinder",
     xlim = c(5, 40), ylim = c(0.,0.26))
lines(density(mpg[cyl==6]), col = "red", lty = 2)
lines(density(mpg[cyl==8]), col = "blue", lty = 3) 
legend("topright", paste(c(4,6,8), "Cylinder"),
       col = c("black","red", "blue"),
       lty = c(1,2,3), lwd = 3, bty ="n")
