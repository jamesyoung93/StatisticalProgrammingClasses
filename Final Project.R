###Final project part 3

setwd("C:\\data\\")
x <- read.csv("project.csv")

hist((x$temp))
hist(log(x$temp))
hist((x$frequency))
hist((x$flow))
hist((x$solids))

par(bg=rgb(1,1,0.8), mfrow=c(1,2))
qqnorm(log(x$temp), main = "log transformed temp")
qqline(log(x$temp), col = 2)
qqnorm((x$temp), main = "non-transformed temp")
qqline((x$temp), col = 2)
boxplot(log(x$temp), horizontal=TRUE, main="Box Plot", xlab="temp")
hist(log(x$temp), main="Histogram", xlab="temp")

library(car)
scatterplotMatrix(x,spread=FALSE,smoother.args=list(lty=2),diagonal="histogram",
                  main="Dependent Variable is Temp")



par(bg=rgb(1,1,0.8),mfrow=c(1,3))
boxplot(temp~frequency, data=x, main="temp by frequency",
        xlab="frequency",ylab="temp")

boxplot(temp~flow, data=x, main="temp by flow",
        xlab="flow",ylab="temp")

boxplot(temp~solids, data=x, main="temp by solids",
        xlab="solids",ylab="temp")


#looking at histograms, I believe it is better to not transform the data as it is non
#continuous except for temp which seems to have a normal distribution

#Linear Model
model <- lm(temp~., data=x)
anova(model)
summary(model)

#adj. R-squared = 0.8745

#Linear Model with log transformation of temp
model2 <- lm(log(temp)~., data=x)
anova(model2)
summary(model2)

#adj. R-squared = 0.9563

#Second order model
model3 <- lm((temp)~ (frequency+flow+solids)^2, data=x)
anova(model3)
summary(model3)

#adj R-squared = 0.9408

#Third order model
model4 <- lm(temp~ (frequency+flow+solids)^3, data=x)
anova(model4)
summary(model4)

#Adj. R-squared = 0.9406

library(caret)
library(ddalpha)
library(ellipse)
library(ggplot2)


# Machine learning model using Support vector machine

control <- trainControl(method="cv", number=10)
metric <- "RMSE"
# SVM
set.seed(7)
fit.svm <- train(temp~., data=x, method="svmRadial", metric=metric, trControl=control)
# summarize Best Model
print(fit.svm)

#Rsquared is 0.989


