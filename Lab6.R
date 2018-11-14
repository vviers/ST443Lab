library(glmnet)
library(ISLR)
Hitters <- na.omit(Hitters)

## glmnet does not use formula language
x <-model.matrix(Salary~.-1, data=Hitters)
y <-Hitters$Salary

## glmnet() function has an alpha argument that determines whaty type of model is fit
## alpha=0 corresponds to the ridge regression and alpha=1 corresponds to the lasso by default

## glmnet() function standardizes the variables so that they are on the same scale. 
fit.ridge <-glmnet(x, y, alpha=0)
## plot of the solution path, i.e. estimated coefficients vs log (lambda), where lambda is the tuning parameter
plot(fit.ridge, xvar="lambda", label= TRUE)
plot(fit.ridge, xvar="dev", label= TRUE)

## k-fold cross validation for to determine the optimal tuning parameter, lambda. By default k is set to be 10
cv.ridge <-cv.glmnet(x, y, alpha=0)
## Plot of CV mse vs log (lambda)
plot(cv.ridge)
## Coefficent vector corresponding to the mse which is within one standard error of the lowest mse using the best lambda.
coef(cv.ridge)
## Coefficient vector corresponding to the lowest mse using the best lambda
coef(glmnet(x, y, alpha=0, lambda=cv.ridge$lambda.min))
# NB: the coefficients are the STANDARDISED ones!!!!!

## Of course, you can try a range of lambda values for the ridge regression
## Here we try a grid of values ranging from lambda=10^10 to lambda=10^{-2}, 100 different lambda values
grid <-10^seq(10, -2, length=100)
fit.ridge1 <-glmnet(x,y,alpha=0, lambda=grid)
cv.ridge1 <- cv.glmnet(x, y, alpha=0, lambda = grid)
plot(cv.ridge1)

## 21 rows (one for each predictor plus an intercept) and 100 columns (one for each value of lambda)
dim(coef(fit.ridge1))
## What is the 50th lambda value and the corresponding estimated coefficients by the ridge regression?
fit.ridge1$lambda[50]
coef(fit.ridge1)[,50]

## Lab 6.2: the lasso
fit.lasso <-glmnet(x,y)
plot(fit.lasso, xvar="lambda", label= TRUE)
plot(fit.lasso, xvar="dev", label= TRUE)
cv.lasso <-cv.glmnet(x, y)
plot(cv.lasso)
## coefficent vector corresponding to the mse which is within one standard error of the lowest mse using the best lambda.
coef(cv.lasso)
cv.lasso$lambda.1se
## coefficient vector corresponding to the lowest mse using the best lambda
coef(glmnet(x,y, lambda=cv.lasso$lambda.min))

## Validation set approach to select best lambda in the lasso
set.seed(1)
train <-sample(seq(263), 180, replace=FALSE)
lasso.train <-glmnet(x[train,], y[train])
lasso.train
pred.test <-predict(lasso.train, x[-train,])
dim(pred.test)
rmse <-sqrt(apply((y[-train]-pred.test)^2,2,mean))
plot(log(lasso.train$lambda), rmse, type="b", xlab="Log(lambda)")
lambda.best <-lasso.train$lambda[order(rmse)[1]]
lambda.best