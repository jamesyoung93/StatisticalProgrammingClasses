#HW 8

data <- read.csv("C:\\data\\HW8.csv")
data
str(data)
data$Factor.C <- as.factor(data$ï..Factor.C)

levels(data$Factor.C)
levels(data$Factor.A)

par(bg=rgb(1,1,0.8), mfrow=c(2,2))
qqnorm(data$bioval)
qqline(data$bioval, col = 2)
boxplot(data$bioval, horizontal=TRUE, main="Box Plot", xlab="bioval")
hist(data$bioval, main="Histogram", xlab="bioval")

par(mfrow=c(1,1))

par(bg=rgb(1,1,0.8),mfrow=c(1,2))
boxplot(bioval~Factor.C, data=data, main="bioval by Factor C",
        xlab="Factor C",ylab="bioval")

boxplot(bioval~Factor.A, data=data, main="bioval by Factor A",
        xlab="Factor A",ylab="bioval")


aov = aov(Response~(Factor.C + Factor.A)^3,data=data)
summary(aov)



model <- lm(Response~(Factor.C*Factor.A),data=data)
anova(model)
summary(model)

with(data, interaction.plot
     (x.factor=Factor.A,trace.factor=Factor.C, 
       bioval=bioval,fun=mean, type="b", ylab="bioval",
       main="Interaction Plot",pch=c(1,19)))


###Problem 11

data <- read.csv("C:\\data\\DATATAB_9_30.csv")
data
str(data)


levels(data$grain)
levels(data$prep)

par(bg=rgb(1,1,0.8), mfrow=c(2,2))
qqnorm(data$bioval)
qqline(data$bioval, col = 2)
boxplot(data$bioval, horizontal=TRUE, main="Box Plot", xlab="bioval")
hist(data$bioval, main="Histogram", xlab="bioval")

par(mfrow=c(1,1))

par(bg=rgb(1,1,0.8),mfrow=c(1,2))
boxplot(bioval~grain, data=data, main="Bioval by Grain",
        xlab="Grain",ylab="bioval")

boxplot(bioval~prep, data=data, main="Bioval by Prep",
        xlab="Prep",ylab="bioval")


aov = aov(bioval~(grain + prep)^2,data=data)
summary(aov)



model <- lm(bioval~(grain + prep)^2,data=data)
anova(model)
summary(model)

with(data, interaction.plot
     (x.factor=grain,trace.factor=prep, 
       response=bioval,fun=mean, type="b", ylab="bioval",
       main="Interaction Plot",pch=c(1,19)))

data$ï..trt <- as.factor(data$ï..trt)
data$ï..trt <- relevel(data$ï..trt, "10") 
summary(lm(bioval~ï..trt,data=data))
str(data)





