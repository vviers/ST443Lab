## Lab 4: Cross Validation and Bootstrap

## Cross Validation
library(ISLR)
Auto = Auto
plot(mpg ~ horsepower, data = Auto)
dim(Auto)
##############################################################################################################################
attach(Auto)
# A validation set approach# Randomly split the full data into half training and half testing
set.seed(1234)
train = sample(x = 392, size = 196)

# linear model: regree mpg on horsepower using training data set
lm_fit = lm(formula = mpg ~ horsepower,
            data = Auto,
            subset = train)

# -train index below selects only the observations that are not in the training set
# MSE OF 196 observations in the validation set
mean((mpg - predict(object = lm_fit, newdata = Auto))[-train] ^ 2)

# we use poly() function to estimate the test erro for the polynomial regressions
lm_fit2 = lm(
  formula = mpg ~ poly(horsepower, 2),
  data = Auto,
  subset = train
)
mean((mpg - predict(object = lm_fit2, newdata = Auto))[-train] ^ 2)

lm_fit3 = lm(
  formula = mpg ~ poly(horsepower, 3),
  data = Auto,
  subset = train
)
mean((mpg - predict(object = lm_fit3, newdata = Auto))[-train] ^ 2)

##############################################################################################################################
library(boot)
# Leave-one-out cv (LOOCV)
glm_fit = glm(formula = mpg ~ horsepower, data = Auto)
# cv.glm() can be used to perform cv, if we use glm() to fit a model without passing in the family argument, then it performs linear regression like lm() function
? cv.glm
cv.glm(Auto, glm_fit)$delta ## Very slow (does not use formula (5.2) on page 180 in ISLR)

# write a simple function to use formua (5.2)
loocv = function(fit) {
  h = lm.influence(fit)$hat
  mean((residuals(fit) / (1 - h)) ^ 2)
}
loocv(glm_fit)

# Plot the cv errors vs degree of the polynomial
cv_error1 = rep(0, 5)
cv_error2 = rep(0, 5)
degree = 1:5
for (i in degree) {
  glm_fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  ## CV errors using formula (5.2)
  cv_error1[i] = loocv(glm_fit)
  ## CV errors using naive LOOCV
  cv_error2[i] = cv.glm(Auto, glm_fit)$delta[1]
}
plot(degree, cv_error1, type = "b")
lines(degree, cv_error2, col = "red")

################################################################################################################################
## 5 fold cross validation, you can try 5 fold CV by setting K=5 in cv_glm function
cv_error5 = rep(0, 5)
for (i in degree) {
  glm_fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  # Note the value of K is the number of groups which the data should be split to estimate the CV error, by default K=n, i.e. LOOCV
  cv_error5[i] = cv.glm(Auto, glm_fit, K = 5)$delta[1]
}
lines(degree, cv_error10, type = "b", col = "blue")

################################################################################################################################
# Bootstrap
# We use Portfolio data set in the ISLR package
# alpha_fn() function takes as input the (X,Y) data as well as a vector indicating which observations should be used to estimate alpha
alpha_fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  # solution of alpha s.t. min. var
  alpha = (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
  return(alpha)
}

set.seed(1)
# randomly select 100 observations from 1 to 100 with replacement, i.e. constract a new bootstrap data and compute the corresponding alpha
alpha_fn(data = Portfolio, index = sample(x = 1:100, size = 100, replace = T))

# Bootstrap using "for" loop:
boot_alpha = rep(0, 1000)
for(i in 1:1000){
  boot_index = sample(x = 1:100, size = 100, replace = T)
  boot_alpha[i] = alpha_fn(data = Portfolio, index = boot_index)
}
mean(boot_alpha)
sd(boot_alpha)

# Use boot() function to produce R=1000 bootstrap estimates for alpha
boot(Portfolio, alpha_fn, R = 1000)

################################################################################################################################
# Estimating the accuracy of a linear regression model
boot_fn = function(data, index) {
  coef(lm(mpg ~ horsepower, data = data, subset = index))
}
# This returns the intercept and slope estimates for the linear regression model
boot_fn(Auto, 1:392)
set.seed(1234)
boot_fn(Auto, sample(392, 392, replace = T))

# Now we use boot() to compute the standard errors of 1000 bootstrap estimates for the intercept and slope
boot(Auto, boot_fn, R = 1000)

# Compare with standard formula results for the regression coefficients in a linear model
summary(lm(mpg ~ horsepower, data = Auto))$coef
# What can you conclude from the different results?

################################################################################################################################
# Redo everything for polynomial regression with degree=2
boot_fn = function(data, index)
  coefficients(lm(
    mpg ~ horsepower + I(horsepower ^ 2),
    data = data,
    subset = index
  ))
set.seed(1)
# Bootstrap with 1000 replications
boot(Auto, boot_fn, 1000)
summary(lm(mpg ~ horsepower + I(horsepower ^ 2), data = Auto))$coef

