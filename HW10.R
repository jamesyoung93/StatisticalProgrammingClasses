
### HW 10

setwd("C:\\data\\")
y <- read.csv("HW10.csv")


par(mfrow=c(1,1))
with(y, interaction.plot
     (x.factor=farm,trace.factor=var, 
       response=weight,fun=mean, type="b", ylab="weight",
       main="Interaction Plot",pch=c(1,19)))

with(y, interaction.plot
     (x.factor=var,trace.factor=farm, 
       response=weight,fun=mean, type="b", ylab="weight",
       main="Interaction Plot",pch=c(1,19)))

