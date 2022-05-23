library(dslabs)
library(dplyr)
data(reported_heights)

# Q1
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]
k_vals <- seq(1, 101, 3)
knn_f1 <- function(k) {
  knn_fit <- knn3(sex ~ height, data = train_set, k=k)
  y_hat_knn <- predict(knn_fit, test_set, type = "class")
  table = confusionMatrix(data = y_hat_knn, reference = test_set$sex)$table
  F_meas(table)
}
f1s <-sapply(k_vals, knn_f1)
max(f1s)

# Q2
library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
test_set <- tissue_gene_expression$x[test_index, ]
test_set_y <- tissue_gene_expression$y[test_index]
train_set <- tissue_gene_expression$x[-test_index, ]
train_set_y <- tissue_gene_expression$y[-test_index]

k_vals <- seq(1, 11, 2)
knn_accuracy_gene_expression <- function(k) {
  knn_fit <- knn3(train_set, train_set_y, k=k)
  y_hat_knn <- predict(knn_fit, test_set, type = "class")
  cm = confusionMatrix(data = y_hat_knn, reference = test_set_y)
  cm$overall["Accuracy"]
}
acc <-sapply(k_vals, knn_accuracy_gene_expression)
acc
