# Lab 5 on Best Subset Selection, Forward and Backward Stepwise Selection,
# Validation Set Approach and K fold Cross Validation

library(ISLR)
? Hitters

summary(Hitters)
head(Hitters$Salary)
## Some missing values in Salary
sum(is.na(Hitters$Salary))

## Remove those missing values
Hitters = na.omit(Hitters)
dim(Hitters)
## Check any missing values?
sum(is.na(Hitters))

#### Lab 5_1: Best Subset Selection
## regsubsets() function perfroms best subset selection by identifying the best model that contains a given number of predictors, where best is quantified by RSS
library(leaps)
regfit_full = regsubsets(Salary ~ ., data = Hitters)
summary(regfit_full)
## It gives by default best-subsets up to size 8; let us increase to 19
regfit_full = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg_summary = summary(regfit_full)
reg_summary
names(reg_summary)
reg_summary$rsq

## Plot RSS, adjusted Rsq, Cp and BIC for all of the models at once, this would help us decide which model to select

par(mfrow=c(2,2))
plot(reg_summary$rss, xlab="Number of Variables", ylab="RSS")

## Plot adjusted R2 vs Number of Variables
plot(reg_summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
## which_max() function is used to identify the location of the maximum point of a vector
(best_model = which.max(reg_summary$adjr2))
## Plot a red dot to indicate the model with the largest adjusted R2
points(best_model, reg_summary$adjr2[best_model], col="red", cex=2, pch=20)

## In a similar fashion, we can plot Cp and BIC
plot(reg_summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
(best_model = which.min(reg_summary$cp))
points(best_model, reg_summary$cp[best_model], col="red", cex=2, pch=20)

plot(reg_summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
(best_model = which.min(reg_summary$bic))
points(best_model, reg_summary$bic[best_model], col="red", cex=2, pch=20)

## regsubsets() function has a build-in plot() command which can display the selected variable for the best model with a given number of predictors
par(mfrow=c(2,2))
plot(regfit_full, scale="r2")
plot(regfit_full, scale="adjr2")
plot(regfit_full, scale="Cp")
plot(regfit_full, scale="bic")

## Top row of each plot contains a black square for each variable selected according to the optimal model associated with that statistic
## e_g_ BIC choose six-variable model we use coef() to see the coefficient estimates associated with this model
coef(regfit_full, id = 6)

##### Lab 5_2: Forward and Backward Stepwise Selection
## regsubsets() function can perform forward or backward stepwise selection as
regfit_fwd = regsubsets(Salary ~ .,
                        data = Hitters,
                        nvmax = 19,
                        method = "forward") # use forward selection


summary(regfit_fwd)
par(mfrow = c(2, 2))
plot(regfit_fwd, scale = "r2")
plot(regfit_fwd, scale = "adjr2")
plot(regfit_fwd, scale = "Cp")
plot(regfit_fwd, scale = "bic")

regfit_bwd = regsubsets(Salary ~ .,
                        data = Hitters,
                        nvmax = 19,
                        method = "backward") # Backwards is problematic when p > n
summary(regfit_bwd)

plot(regfit_bwd, scale="r2")
plot(regfit_bwd, scale="adjr2")
plot(regfit_bwd, scale="Cp")
plot(regfit_bwd, scale="bic")

## Check the coefficient estimates associated with models (size 7) using different approaches, e_g_ best subsec selection, forward stepwise selection and backward stepwise selection
coef(regfit_full, id =7)
coef(regfit_fwd, 7)
coef(regfit_bwd, 7)

#### Lab 5_3 Choosing among models using Validation Set Approach and Cross-Validation
set.seed(1)

## Validation Set Approach, randomly split the data into training set and validation data
train = sample(seq(263), 180, replace=FALSE)
train
## We now apply regsubsets() on the training data to perform forward stepwise selection
regfit_fwd = regsubsets(Salary ~ .,
                        data = Hitters[train, ],
                        nvmax = 19,
                        method = "forward")

val_errors = rep(0, 19)
## We make a model matrix from the testing data
test_mat = model.matrix(Salary ~ ., data = Hitters[-train, ])

## Try all models with size i ranges from 1 to 19
for(i in 1:19){
  ## Get the coefficient estimates associated with model (size i) using forward stepwise method
  coef_i = coef(regfit_fwd, id = i)
  ## Get the prediction for the tesing data using the corresponding columns in the design matrix X multipled by the estimated coefficients in coef_i 
  pred_test = test_mat[, names(coef_i)] %*% coef_i
  ## Compute the mean square error
  val_errors[i] = mean((Hitters$Salary[-train] - pred_test) ^ 2)
}
which.min(val_errors)
coef(regfit_fwd, 5)

par(mfrow=c(1,1))
## Plot of Root MSE vs model size for validation data
plot(sqrt(val_errors) ,ylab="Root MSE", ylim=c(300,400), pch=19, type="b")
## Plot of Root MSE vs model size for training data
points(sqrt(regfit_fwd$rss[-1]/180), col="red", pch=19, type="b")
legend("topright",legend=c("Validation","Training"), col=c("black","red"), pch=19)

## There is no predict() method for regsubsets(), we summarize the steps for our computation above and write our own version of the predict function as
predict_regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]]) # get model formula
  mat = model.matrix(object = form, data = newdata)
  coef_i = coef(object, id = id)
  mat[, names(coef_i)] %*% coef_i
}

## Repeat the above steps for computing test error rate for models with size from 1 to 19
val_errors2 =rep(0, 19)
for(i in 1:19){
  val_errors2[i] = mean((Hitters$Salary[-train] - predict_regsubsets(regfit_fwd, Hitters[-train,], id=i))^2)  
}
## Check whether our written function could provide the same result or not
sum(abs(val_errors2 - val_errors))


## K-Cross Validation Approach using forward stepwise selection (FSS), this part is very important, since it provides you a sample code of writing K-fold CV
K =10
set.seed(11)
folds = sample(rep(1:10, length = nrow(Hitters)))
folds
table(folds)
## We initialize a error matrix with row (10 different folds) and column (19 different predictors)
cv_errors = matrix(0, 10, 19)
## We write a for loop that performs cross-validation, in the kth fold, the elemetns of folds that equal k are in the test set and the remiander are in the training set
for(k in 1:10){
  fit_fwd = regsubsets(Salary~., data=Hitters[folds!=k,], nvmax=19, method="forward")
  for(i in 1:19){
    pred =predict_regsubsets(fit_fwd, Hitters[folds==k,], id=i)
    cv_errors[k,i] =mean((Hitters$Salary[folds==k]-pred)^2)
  }
}

## Average of the cv_error over all 10 folds
rmse_cv = sqrt(apply(cv_errors,2,mean))
## Plot of Root MSE vs model size and choose the optimal model size
plot(rmse_cv, ylab="Root MSE", xlab="Model Size", pch=19, type="b")
which.min(rmse_cv)
points(which.min(rmse_cv), rmse_cv[which.min(rmse_cv)], col="red", cex=2, pch=20)