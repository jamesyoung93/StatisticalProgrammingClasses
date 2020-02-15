#Problem 1
x <- read.csv("C:\\data\\datatab_7_16.csv")
summary(x)


model <- lm(final ~ ï..midterm  , data=x)
summary(model)
coef(model)
anova(model)
confint(model,level=0.95)
plot(model)

shapiro.test(model$residuals)

sxx <- sum((x$ï..midterm-mean(x$ï..midterm))^2)
sxx

par(mfrow=c(1,1))
plot(x$ï..midterm,x$final,xlab="Midterm Score", ylab="Final Score",
     ylim=c(0,120),xlim=c(20,100),main="Chapter 7 Linear Regression Example",
     pch=19,cex=1.5)
lines(sort(x$ï..midterm),fitted(model)[order(x$ï..midterm)], col="blue", type="l")


grades <- data.frame(ï..midterm=c(80,90,95,99))
predict(model, grades, interval="predict")

grades <- data.frame(ï..midterm=c(82))
predict(model, grades, interval="predict")

#Problem 2
x <- read.csv("C:\\data\\datatab_7_22.csv")
summary(x)
t <- split(x, x$ï..drug)
drugA <- t$A
drugG <- t$G

model <- lm(drugA$halflife ~ drugA$do_mg_kg  , data=x)
summary(model)
model2 <- lm(drugG$halflife ~ drugG$do_mg_kg  , data=x)
summary(model2)
coef(model)
anova(model)
anova(model2)
confint(model,level=0.95)
plot(model)
plot(model2)

library(car)
scatterplot(halflife~do_mg_kg | ï..drug, data=x,
            xlab="Drug Dosage", ylab="Half-Life",
            main="Enhanced Scatter Plot",
            labels=row.names(x))
abline(model)
abline(model2)

shapiro.test(model$residuals)
shapiro.test(model2$residuals)

#TSS

t1 <- sum(drugA$halflife^2)
t1

t2 <- (sum(drugA$halflife)^2)/length(drugA$halflife)
t2

#TSS= t1-t2

tss <- t1-t2
tss

#Calculate Sxx

sxx <- sum((drugA$do_mg_kg-mean(drugA$do_mg_kg))^2)
sxx

sxx <- sum((drugG$do_mg_kg-mean(drugG$do_mg_kg))^2)
sxx
