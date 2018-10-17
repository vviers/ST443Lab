library(ISLR)

smarket <- Smarket

dim(smarket)
summary(smarket)
pairs(smarket)

cor(smarket) #Error in cor(smarket) : 'x' must be numeric

cor(smarket[-9])

class(smarket$Direction) # factor (0 and 1 under the hood)

# Logistic Regression

glm_fit <- glm(Direction ~ .-Year -Today,
               data = smarket,
               family = binomial(link = "logit"))
summary(glm_fit)
# remove Year,
# remove Today because we would always predict the correct one (up/down if we knew yesterday and today)


# `type = "response"` means answer is on the scale of the response variable (here, probabilities)
# the default `type = "link"` would give the log-odds (probabilities on logit scale), see `?predict.glm`
glm_probs <- predict(glm_fit, type = "response")
glm_probs[1:10]

glm_logodds <- predict(glm_fit)
exp(glm_logodds) / (1 + exp(glm_logodds)) # same as:
glm_probs


# "Homemade" Confusion Matrix
contrasts(smarket$Direction)
# Set predicted prob > .5 to "Up" and the rest to "Down"
glm_pred = rep("Down", 1250)
glm_pred[glm_probs > .5] = "Up"

table(glm_pred, smarket$Direction) # confusion matrix
(507+145)/1250   # 0.5216 accuracy


# Testing - training split

train = smarket$Year < 2005
test = !smarket$Year < 2005

smarket_test = smarket[test, -9]
direction_test = smarket[test, 9]

glm_fit <- glm(Direction ~ .-Year -Today,
               data = smarket,
               family = binomial(link = "logit"),
               subset = train) # only fit the training data

glm_prob = predict(glm_fit, newdata = smarket_test, type = "response")

glm_pred = rep("Down", length(direction_test))
glm_pred[glm_prob > .5] = "Up"

table(glm_pred, direction_test)
(77 + 44)/length(direction_test) #48% test accuracy...






# Linear Discriminant Analysis
# LDA on smarket using only 2 predictors: lag1 and lag2
library(MASS)

lda_fit = MASS::lda(Direction ~ Lag1 + Lag2, data = smarket, subset = train)
lda_fit

str(predict(lda_fit, smarket_test))

lda_pred_posterior = predict(lda_fit, smarket_test)$posterior
lda_pred = predict(lda_fit, smarket_test)$class

contrasts(lda_pred) == contrasts(direction_test) #ok

table(lda_pred, direction_test)

mean(lda_pred == direction_test) # 56% now :)


# Look at data:
library(ggplot2)

ggplot(data = smarket, aes(x = Lag1, y = Lag2, color = Direction)) +
  geom_point()


# Lab 3 Quadratic Discriminant Analysis
# Perform QDA on the traning data set using only two predictors, 1st lag and 2nd lag
qda_fit = qda(Direction ~ Lag1 + Lag2, data = smarket, subset = train)
qda_fit
qda_pred = predict(qda_fit, smarket_test)$class

# Confusion matrix
table(qda_pred, direction_test)
# Misclassfication error rate
mean(qda_pred != direction_test) #60% accuracy


# Lab 3 k Nearest Neighbors
# Perform K-nearest neighbours on the traning data set
library(class)
# Create training data for X
train_X = cbind(smarket$Lag1, smarket$Lag2)[train, ]
# Create testing data for X
test_X = cbind(smarket$Lag1, smarket$Lag2)[!train,]
# Create training data for Y
train_Direction = smarket$Direction[train]

# Set k=1
knn_pred = knn(train_X, test_X, train_Direction, k = 1)
table(knn_pred, direction_test)
mean(knn_pred != direction_test) # 50%

# Set k=3
knn_pred = knn(train_X, test_X, train_Direction, k = 3)
table(knn_pred, direction_test)
mean(knn_pred == direction_test)

# kNN

