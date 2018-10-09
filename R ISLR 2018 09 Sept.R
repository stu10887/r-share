### This script is for use with a collaborative adult learning seminar
###    with the Lads and Lasses who R user group.
###    Some of this code was used from the website: http://www-bcf.usc.edu/~gareth/ISL/data.html
###    and used with permission of the authors: G. James, D. Witten,  T. Hastie and R. Tibshirani .
###    Original code is shared through a Creative Commons Attribution + ShareAlike license (BY-SA) by Author: Stu Rodgers, 2018

install.packages(c(“ISLR"), repos='http://cran.us.r-project.org')
library(“ISLR")
data()
summary(Wage)

### vectors, data, matrices, aarays, subsetting
x <- c(2,7,5)
x
y <- seq(from=4,length=3,by=3)
?seq
y
x+y
x/y
x^y
x[2]
x[2:3]
x[-2]
x[-c(1,2)]
## Make a matrix with 4 rows, 3 columns
z <- matrix(seq(1,12),4,3)
z
z[3:4,2:3]
z[,2:3]
z[,1]
## For matrices, drop=FALSE will preserve the martix dimensions
z[,1,drop=FALSE]
dim(z)
array(letters, c(2,4,3))
array(letters, c(3,4,3))
ls()
rm(y)
ls()

### Generating random data, graphics
x <- runif(50)
y <- rnorm(50)
## plot is the basic plotting function
plot(x,y)
plot(x,y,xlab="Random Uniform",ylab="Random Normal",pch="*",col="blue")
par(mfrow=c(2,1))
plot(x,y)
hist(y)
par(mfrow=c(1,1))
### Setting the working directory
setwd("C:/Users/stu/ownCloud/ISLR")


### Reading in data, Auto.csv is a flat text file in csv format
Auto <- read.csv("Auto.csv")
### Explore the data
names(Auto)
dim(Auto)
class(Auto)
summary(Auto)
plot(Auto$cylinders,Auto$mpg)
plot(Auto$cyl,Auto$mpg)
attach(Auto)
search()
plot(cylinders,mpg)
cylinders=as.factor(cylinders)
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")


## Create a pdf file in the current working directory
pdf(file="mpg.pdf")
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
dev.off()
## Produce a matrix of scatterplots. The ijth scatterplot contains x[,i] plotted against x[,j]
pairs(Auto,col="brown")


library(MASS)
### Simple linear regression
names(Boston)
?Boston
## medv: median value of owner-occupied homes in $1000s
## lstat: percent of households with low socioeconomic status
plot(medv~lstat,Boston)
fit1 <- lm(medv~lstat,data=Boston)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")


### Multiple linear regression
fit2 <- lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3 <- lm(medv~.,Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
fit4 <- update(fit3,~.-age-indus)
summary(fit4)


### Nonlinear terms and Interactions
fit5 <- lm(medv~lstat*age,Boston)
summary(fit5)
fit6 <- lm(medv~lstat +I(lstat^2),Boston); summary(fit6)
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)
fit7 <- lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)


### Qualitative predictors
## Predict Sales (child car seat sales) in 400 locations based on a number of predictors?
fix(Carseats)
names(Carseats)
summary(Carseats)
fit1 <- lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)


### Writing R functions - a very simple start
regplot <- function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
regplot <- function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)




