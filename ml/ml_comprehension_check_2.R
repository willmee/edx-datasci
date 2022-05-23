library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Q8
features = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')
max_accuracy_train <- function(feature) {
  feature_range <- range(train[feature])
  cutoff <- seq(feature_range[1], feature_range[2], by = 0.1)
  accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(train[feature] > x, "virginica", "versicolor") %>% 
      factor(levels = levels(iris$Species))
    mean(y_hat == train$Species)
  })
  c(max(accuracy), cutoff[which.max(accuracy)])
}
sapply(features, max_accuracy_train)

# Q9
# For the feature selected in Q8, use the smart cutoff value from the training data to calculate overall accuracy in the test data. What is the overall accuracy?
feature = 'Petal.Length'
y_hat <- ifelse(train[feature] > 4.8, "virginica", "versicolor") %>% 
  factor(levels = levels(iris$Species))
mean(y_hat == train$Species)
mean(y_hat == test$Species)

# Q10
max_accuracy <- function(feature) {
  feature_range <- range(test[feature])
  cutoff <- seq(feature_range[1], feature_range[2], by = 0.1)
  accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(test[feature] > x, "virginica", "versicolor") %>% 
      factor(levels = levels(iris$Species))
    mean(y_hat == test$Species)
  })
  max(accuracy)
  #which.max(accuracy)
  #cutoff[which.max(accuracy)]
}

sapply(features, max_accuracy)

# Q11
plot(iris,pch=21,bg=iris$Species)

y_hat <- ifelse(test$Petal.Length > 4.7 | test$Petal.Width > 1.50, "virginica", "versicolor") %>% 
  factor(levels = levels(iris$Species))
mean(y_hat == test$Species)
