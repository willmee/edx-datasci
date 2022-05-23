options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

# Q1
class(brca$x)
dim(brca$x)
mean(brca$y == 'M')
which.max(apply(brca$x, 2, mean))
which.min(apply(brca$x, 2, sd))

# Q2
# sweep(brca$x, 2, apply(brca$x, 2, mean))
x_scaled <- with(brca, {sweep(x, 2, apply(x, 2, mean))})
x_scaled <- sweep(x_scaled, 2, apply(brca$x, 2, sd), "/")
sd(x_scaled[,1])
median(x_scaled[,1])

# Q3
x_dist <- as.matrix(dist(x_scaled))
x_dist[1,]
brca$y == 'B'
mean(x_dist[1,brca$y == 'B'][-1])
mean(x_dist[1,brca$y == 'M'])

# Q4
features_dist <- as.matrix(dist(t(x_scaled)))
heatmap(features_dist, labRow = NA, labCol = NA)

d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

# Q5
clust <- hclust(d_features)
# groups has feature names like 'perimeter_mean'
groups <- cutree(clust, k=5)
split(names(groups), groups)

# Q6
pca <- prcomp(x_scaled)
summary(pca)

# Q7
as.data.frame(pca$x[,1:2]) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color=brca$y))

# Q8
boxplot(as.data.frame(pca$x[,1:10]), col=brca$y)
for(i in 1:10) {
  boxplot(pca$x[,i] ~ brca$y, main = paste("PC", i))
}


# set.seed(1) if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

# Q9
mean(test_y == 'M')
mean(train_y == 'M')

# Q 10 kmeans
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

set.seed(3, sample.kind = "Rounding") 
k <- kmeans(train_x, 2)
km_predict <- predict_kmeans(test_x, k)
predict(k, data.frame(test_x))
km_predict <- ifelse(km_predict == 2, "M", "B")
mean(km_predict == test_y)
# 0.922

# Q10b
confusionMatrix(factor(km_predict), factor(test_y), positive='M')

# Q11 logistic regression
set.seed(3, sample.kind = "Rounding")
model_glm <- train(train_x, train_y, method='glm', family='binomial')
lr_y_hat <- predict(model_glm, data.frame(test_x))
mean(lr_y_hat == test_y)
# 0.957

# Q12 LDA & QDA
set.seed(1, sample.kind = "Rounding")
model_lda <- train(train_x, train_y, method='lda')
y_hat <- predict(model_lda, data.frame(test_x))
mean(y_hat == test_y)
# 0.991

set.seed(1, sample.kind = "Rounding")
model_qda <- train(train_x, train_y, method='qda')
y_hat <- predict(model_qda, data.frame(test_x))
mean(y_hat == test_y)
# 0.957

# Q13 Loess
set.seed(5, sample.kind = "Rounding")
model_loess <- train(train_x, train_y, method='gamLoess')
y_hat <- predict(model_loess, data.frame(test_x))
mean(y_hat == test_y)
# 0.983

# Q14 - KNN
set.seed(7, sample.kind = "Rounding")
ks <- seq(3, 21, 2)
knn_accuracy <- map_df(ks, function(k){
  fit <- knn3(train_x, train_y, k = k)
  
  y_hat <- predict(fit, train_x, type = "class")
  cm_train <- confusionMatrix(y_hat, train_y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, test_x, type = "class")
  cm_test <- confusionMatrix(y_hat, test_y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(k = k, train = train_error, test = test_error)
})
knn_accuracy
which.max(knn_accuracy$train)


set.seed(7, sample.kind = "Rounding")
model_knn <- train(train_x, train_y, 
                   method = "knn", 
                   tuneGrid = data.frame(k = seq(3, 21, 2)))
model_knn$finalModel
y_hat <- predict(model_knn, data.frame(test_x))
mean(y_hat == test_y)
# 0.957

# Q15 Random Forest
set.seed(9, sample.kind = "Rounding")
model_rf <- train(train_x, train_y, 
                   method = "rf", 
                   importance = TRUE,
                   tuneGrid = data.frame(mtry = c(3, 5, 7, 9)))
model_rf$bestTune
y_hat <- predict(model_rf, data.frame(test_x))
mean(y_hat == test_y)
# 0.983
varImp(model)

# Q16
models=c(model_glm, model_lda, model_qda, model_loess, model_knn, model_rf)
names(models) <- c('lr', 'lda', 'qda', 'loess', 'knn', 'rf')

gen_predict <- function(model) {
  #str(model)
  predict(model, newdata = data.frame(test_x))
}  

# can't get this to work because the objects in models aren't the actual models
# :-(
sapply(models, gen_predict, simplify = TRUE, USE.NAMES = TRUE)
gen_predict(models[[1]])

ensemble_y_hats <- rbind(
  predict_kmeans(test_x, k),
  gen_predict(model_glm),
  gen_predict(model_lda),
  gen_predict(model_qda),
  gen_predict(model_loess),
  gen_predict(model_knn),
  gen_predict(model_rf)
)

ensemble_y_hat <- as.factor(apply(ensemble_y_hats, 2, function(x) {ifelse(sum(x==1) >= 4, 'B', 'M')})) 
mean(ensemble_y_hat == test_y)
# 0.983

# model answer does something similar:
ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)


