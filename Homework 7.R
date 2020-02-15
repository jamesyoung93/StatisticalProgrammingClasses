#Homework 7

z <- read.csv("C:\\data\\DATATAB_LAKES.csv")
x <- read.csv("C:\\data\\WinterLakes.csv")
y <- read.csv("C:\\data\\SummerLakes.csv")

library(car)
scatterplotMatrix(x[,6:8],spread=FALSE,smoother.args=list(lty=2),diagonal="histogram",
                  main="Winter Months")

scatterplotMatrix(y[,6:8],spread=FALSE,smoother.args=list(lty=2),diagonal="histogram",
                  main="Summer Months")
wintermodel <- lm(wtrCHLO ~ wtrTN + wtrTP , data = x)
summary(wintermodel)
summermodel <- lm(smrCHLO ~ smrTN + smrTP , data = y)
summary(summermodel)

plot(wintermodel)
plot(summermodel)
shapiro.test(wintermodel$residuals)
shapiro.test(summermodel$residuals)
