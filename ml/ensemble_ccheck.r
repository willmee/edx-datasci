library(caret)
library(dslabs)
library(tidyverse)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

# Q1
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

# Q2
# my solution looks up the fit from fits, but needs to be converted from a 
# list of lists to a list 
model_predict <- function(model) {
  fit <- fits[model]
  y_hat <- predict(fit, mnist_27$test, type="raw")
  y_hat[[1]]
}

predictions <- sapply(models, model_predict, simplify = TRUE, USE.NAMES = TRUE)
dim(predictions)

# the model soluton is simpler and passes in fits to sapply
model_predict_2 <- function(fit) {
  predict(fit, newdata = mnist_27$test)
}
predictions_2 <- sapply(fits, model_predict_2, simplify = TRUE, USE.NAMES = TRUE)
dim(predictions_2)

# Q3
mean(predictions == mnist_27$test$y)

# given answer uses colMeans
acc <- colMeans(predictions == mnist_27$test$y)
acc
mean(acc)

# Q4
majority <- function(x) {
  ifelse(sum(x == "7" ) > length(x)/2, "7", "2")
}
majority_ensemble_pred <- apply(predictions, 1, majority)

ensemble_acc <- mean(majority_ensemble_pred == mnist_27$test$y)  
ensemble_acc

# Q5
sum(acc > ensemble_acc)
acc > ensemble_acc

# Q6
train_acc <- sapply(fits, function(fit) {mean(fit$results$Accuracy)[1]}, simplify = TRUE)
train_acc
mean(train_acc)

# Q7
best_models <- train_acc > 0.80
#predictions[1,]
#predictions[1, train_acc > 0.80]
pred_best_models <- predictions[,best_models]

majority_best_ensemble_pred <- apply(pred_best_models, 1, majority)
majority_best_ensemble_acc <- mean(majority_ensemble_pred == mnist_27$test$y)  


