##take home test
x <- read.csv("C:\\data\\take_home_test_1.csv")
x

summary(x)

#structure and descriptive stats
str(x)
summary(x)
boxplot(Response~ï..Treatment, data=x)

##Fit a linear Model to extract the ANOVA table

model <- lm(Response~ï..Treatment, data=x)
model
summary(model)
anova(model)

#Plot the groups using plot

plot(Response~ï..Treatment,data=x)

#Examine the residuals

#histogram, qqplot, and boxplots of residuals

hist(model$residuals)
qqnorm(model$residuals)
qqline(model$residuals)
boxplot(model$residuals)

##Test for Normality

shapiro.test(model$residuals)

#test for Homogeneity of Variance

library(car)
leveneTest(Response~ï..Treatment,data=x)

c1 <- c(3, -1, -1, -1)
mat <- c1
mat
contrasts(x$ï..Treatment) <- mat
model1 <- aov(Response~ï..Treatment, data = x)
model1
summary(model1)

summary.aov(model1, split=list(ï..Treatment=list("Control vs. 1,2,3 Hour/Day"=1))) 


library(psych)
library(agricolae)

comparison <- duncan.test(model,"ï..Treatment")
comparison
comparison$groups
