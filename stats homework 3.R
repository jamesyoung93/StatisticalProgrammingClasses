Problem 1
data <- read.csv("C:\\data\\datatab_6_28.csv", header = TRUE)

data
str(data)
colnames(data)
data$group <- as.factor(data$ï..group)


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

 Make sure to use summary.aov here or 'split' might not work

summary.aov(model1, split=list(group=list("Canines vs. Felines"=1, "Cougars vs House Cats" = 2, "Wolves vs Dogs"=3))) 



#ploynomial Contrast


# clear the environment

rm(list=ls())

# read in Data Set #2

data <- read.csv("c:\\data\\RClub_DataSet2_11.3.15.csv", header = TRUE)
data
str(data)



#install.packages("dplyr")
library(dplyr) 

brary(dplyr) 

groups <- group_by(data, dose) # this just prepares it for us to calculate eveyrthing within each condition

plot.data <- summarise(groups,
  mean = mean(score, na.rm=TRUE),
  sd = sd(score, na.rm=TRUE),
  n = n(),
  se=sd/sqrt(n),
  ci = qt(0.975, df=n-1)*se)
plot.data # take a peek



install.packages("ggplot2")
library(ggplot2)
ggplot(plot.data, aes(x=dose, y=mean, group = factor(1))) +
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1) +
  ggtitle("Figure 1. Mean Score by Dose")



### Polynomial Contrasts

For additional info on setting up contrasts (beyond the scope of this lab), check out the ever useful [UCLA stats walkthrough](http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm)
# run an ANOVA 

results <- aov(score ~ dose, data=data)
summary.aov(results)


# Run a Polynomial Contrast

# Here's one way - tell R which groups to compare. These are the orthogonal polynomial contrasts. 

c1 <- c(-1, 0, 1)
c2 <- c(0.5, -1, 0.5)

# combined the above 2 lines into a matrix
mat <- cbind(c1,c2)

# tell R that the matrix gives the contrasts you want
contrasts(data$dose) <- mat

# these lines give you your results

anova(lm(score ~ dose, data = data))

model1 <- aov(score ~ dose, data = data)
summary.aov(model1, split=list(dose=list("Linear"=1, "Quadratic" = 2)))












