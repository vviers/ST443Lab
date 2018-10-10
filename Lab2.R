library(MASS)
library(ISLR)

# we use the Boston dataset

lm.fit <- lm(medv ~ lstat, data = Boston)
summary(lm.fit)

# lm.fit is a List a 12 elements whose names are
names(lm.fit)
str(lm.fit)

# they can be accessed using 
lm.fit$coefficients
coef(lm.fit) # another way to do it

# Confidenc interval for the coefficient estimates
confint(lm.fit)

# Higher Dimentional Tensors
array(1:12, dim = c(2, 2, 3))
# note that vectors are one-dimensional arrays
#           matrices are two-dimensional arrays

# Preditive values, confidence intervals and prediction intervals for the prediction of medv for a given value of lstat
predict(lm.fit, data.frame(lstat = c(5, 10, 15)))

predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "confidence")

predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), interval = "prediction")
# ConfInt(y_bar_hat) = CI(beta0_hat + x * beta1_hat)
# PredInt(y_hat) = CI(beta0_hat + x * beta1_hat + e)

plot(lstat ~ medv, data = Boston)
abline(lm.fit)
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat ~ medv, Boston, col = "red")
plot(lstat ~ medv, Boston, pch = 20)
plot(lstat ~ medv, Boston, pch = "+")

# Regression Diagnostic
par(mfrow = c(2, 2)) # change plotting param to 2x2 frames
plot(lm.fit)
# "NICE": normality, independent, constant variance, E{e} = 0


par(mfrow = c(1, 1))
# plot of fitted values vs (standardized) residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
# plot of leverage statistics
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

#Lab 2 Multiple Linear Regression
lm.fit1 = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit1)
lm.fit2 = lm(medv ~ ., data = Boston)
summary(lm.fit2)
lm.fit3 = lm(medv ~ . - age, data = Boston)
summary(lm.fit3)
lm.fit4 <- lm(medv ~ lstat + age + tax + rad, data = Boston)
summary(lm.fit4)
anova(lm.fit1, lm.fit4)  ## F test, anova() function performs a hypothesis test comparing the two models

summary(lm(medv ~ . - age + lstat:black, data = Boston))
summary(lm(medv ~ . - age + I(rm ^ 2), data = Boston))

summary(lm(medv ~ . - age + log(rm), data = Boston))
summary(lm(medv ~ lstat * age, data = Boston))


# Lab 2 Categorical Predictors

?Carseats
names(Carseats)
# Predict sales (child car seat sales) in 400 locations on a number of predictors
lm.fit = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

attach(Carseats) # we can access variables in Carseats without the `$`
# ShelveLoc: an indicator of the quality of the shelving location, i.e.
# the space within a store in which the car seat is displayed at each location
contrasts(ShelveLoc)
contrasts(Urban)
