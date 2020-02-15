##Problem 1
data <- read.csv("C:\\data\\datatab_6_28.csv", header = TRUE)
## What doth life
data
str(data)
colnames(data)
data$group <- as.factor(data$ï..group)


# Create factor variable to use in analysis
data$group <- as.factor(data$ï..group)
class(data$group)

# Use lm function to analyze the data
result <- lm(los~group,data=data)
anova(result)  # Compare to Table 6.6 on page 257 in our text


#####################################################################################
#
# Updated with Tukey and Duncan Tests
#

library(psych)
library(agricolae)

comparison <- duncan.test(result,"group")
comparison
comparison$groups

comparison.Tukey <- HSD.test(result,"group",group=TRUE)
comparison.Tukey                      
comparison.Tukey$groups

#install.packages("psych")
install.packages("psych")
library(psych)


describeBy(data$los, data$ï..group, mat=TRUE)


# run an omnibus ANOVA

model <- aov(los ~ ï..group, data = data)
summary(model)

# look at the levels of our factor

levels(data$group)


# tell R which groups to compare

c1 <- c(0, 1, -1,0) #B vs. C
c2 <- c(1, 1, -1, -1) #A and B vs. C and D
c1
c2
# combined the above 3 lines into a matrix
mat <- cbind(c1,c2)
mat

# tell R that the matrix gives the contrasts you want
contrasts(data$group) <- mat

# these lines give you your results
model1 <- aov(los ~ group, data = data)
model1
summary(model1)


summary.aov(model1, split=list(group=list("B vs C"=1, "A and B vs C and D" = 2))) 






##Problem 2
x <- read.csv("C:\\data\\DATATAB_6_36.csv")
x
summary(x)

x$ï..nitrogen <- as.factor(x$ï..nitrogen)
x$ï..nitrogen
boxplot(wheat~ï..nitrogen, data=x)

model <- lm(wheat~ï..nitrogen, data=x)
model
summary(model)
anova(model)
qqnorm(model$residuals)
qqline(model$residuals)
shapiro.test(model$residuals)

#install.packages("psych")

library(psych)


describeBy(data$los, data$ï..group, mat=TRUE)


# run an omnibus ANOVA

model <- aov(los ~ ï..group, data = data)
summary(model)

# look at the levels of our factor

levels(data$group)


# tell R which groups to compare

c1 <- c(.5, -.5, .5, -.5) # canines vs. felines
c2 <- c(1, 0, -1, 0) # cougars vs. housecats
c3 <- c(0, 1, 0, -1) # dogs vs. wolves
c1
c2
c3

# combined the above 3 lines into a matrix
mat <- cbind(c1,c2,c3)
mat

# tell R that the matrix gives the contrasts you want
contrasts(data$group) <- mat

# these lines give you your results
model1 <- aov(los ~ group, data = data)
model1
summary(model1)


summary.aov(model1, split=list(group=list("Canines vs. Felines"=1, "Cougars vs House Cats" = 2, "Wolves vs Dogs"=3))) 



#ploynomial Contrast


# clear the environment

rm(list=ls())

# read in Data Set #2

data <- read.csv("c:\\data\\DATATAB_6_36.csv", header = TRUE)
data
str(data)



#install.packages("dplyr")
library(dplyr) 

library(dplyr) 

data$nitrogen <-as.factor(data$ï..nitrogen)


groups <- group_by(data, nitrogen) 
# this just prepares it for us to calculate eveyrthing within each condition

plot.data <- summarise(groups,
                       mean = mean(wheat, na.rm=TRUE),
                       sd = sd(wheat, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975, df=n-1)*se)
plot.data # take a peek



### Polynomial Contrasts

For additional info on setting up contrasts (beyond the scope of this lab), check out the ever useful [UCLA stats walkthrough](http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm)
# run an ANOVA 

results <- aov(wheat~nitrogen, data=data)
summary.aov(results)


# Run a Polynomial Contrast

# Here's one way - tell R which groups to compare. These are the orthogonal polynomial contrasts. 

c1 <- c(-5,-3,-1, 1, 3,5)
c2 <- c(5,-1,-4,-4,-1,5)

# combined the above 2 lines into a matrix
mat <- cbind(c1,c2)

# tell R that the matrix gives the contrasts you want
contrasts(data$nitrogen) <- mat

# these lines give you your results

anova(lm(wheat~nitrogen, data = data))

model1 <- aov(wheat ~ nitrogen, data = data)
summary.aov(model1, split=list(nitrogen=list("Linear"=1, "Quadratic" = 2)))
