x <- read.csv("C:\\data\\datatab_6_28.csv")
x

summary(x)

#structure and descriptive stats
str(x)
summary(x)
x$ï..group <- as.factor(x$ï..group)
x$ï..group
boxplot(los~ï..group, data=x)

##Fit a linear Model to extract the ANOVA table

model <- lm(los~ï..group, data=x)
model
summary(model)
anova(model)

#Plot the groups using plot

plot(los~ï..group,data=x)

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
leveneTest(los~ï..group,data=x)


#Problem 2

y <- read.csv("C:\\data\\datatab_6_35.csv")
y

summary(y)

#structure and descriptive stats
str(y)
summary(y)
boxplot(nodead~ï..insect, data=y)

##Fit a linear Model to extract the ANOVA table

model2 <- lm(nodead~ï..insect, data=y)
model2
summary(model2)
anova(model2)

#Plot the groups using plot

plot(nodead~ï..insect,data=y)

#Examine the residuals

#histogram, qqplot, and boxplots of residuals

hist(model2$residuals)
qqnorm(model2$residuals)
qqline(model2$residuals)
boxplot(model2$residuals)

##Test for Normality

shapiro.test(model2$residuals)

#test for Homogeneity of Variance

library(car)
leveneTest(nodead~ï..insect,data=y)

