# Homework 5

y <- read.csv("C:\\data\\DATATAB_8_31.csv")
y$ï..obs = NULL

library(car)
scatterplotMatrix(y,spread=FALSE,smoother.args=list(lty=2),diagonal="histogram",
                  main="Dependent Variable is Time")


# Part (a) Linear regression for estimating sales price

model <- lm(time ~ ., data=y)
abline(model)
summary(model)
coef(model)
anova(model)
confint(model,level=0.95)

time <- data.frame(clot=c(3.2), prog=c(78), enz=(120), liv=(3))
predict(model, time, interval="predict")


plot(model)
shapiro.test(model$residuals)
summary(model)

model2 <- lm(log(time) ~ ., data=y)
coef(model2)
anova(model2)
confint(model2,level=0.95)
summary(model2)

results <- anova(model, model2)
results
reduced <- lm(log(time) ~ ., data=y) # Reduced model
full <- lm(time ~ ., data=y) # Full Model
anova(reduced, full)

reduced <- lm(time ~ enz + liv, data=y) # Reduced model
full <- lm(time ~ clot + prog + enz + liv, data=y) # Full Model
anova(reduced, full)

results = lm(Price ~ Size + Lot, data=Housing)
predict(results,data.frame(Size=1000, Lot=20000),interval="confidence")



# predicted values
pred.values2 <- fitted(model2)
pred.values2

# residuals
resid.values2 <- residuals(model2)
resid.values2

par(mfrow = c(1, 1))

# scatter plot of residuals versus predicted values
plot(pred.values2,resid.values2)



# Part (b) Linear regression for estimating ln(sales price)

# Now analyze natural logarithm of price
data$ln_price <- log(data$price)
data$ln_price
data$price

scatterplotMatrix(~ln_price + age + bed + bath + size + lot ,data=data,
                  spread=FALSE, smoother.args=list(lty=2),diagonal="histogram",
                  main="Dependent Variable is ln(Price)")

model3 <- lm(ln_price ~ age + bed + bath + size + lot, data=data)
summary(model3)
anova(model3)
confint(model3,level=0.95)

# examine residuals
par(mfrow = c(2, 2))  # set the plotting window for four graphs
plot(model3)

# predicted values
pred.values3 <- fitted(model3)
pred.values3

# residuals
resid.values3 <- residuals(model3)
resid.values3

par(mfrow = c(1, 1))

# scatter plot of residuals versus predicted values
plot(pred.values3,resid.values3)










