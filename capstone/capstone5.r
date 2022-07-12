library(stringr)
library(tidyverse)
library(caret)

load(file="/Users/willmee/dev/rlang/edx-datasci/data/movielens-capstone.Rda")
load(file="/Users/willmee/dev/rlang/edx-datasci/data/movielens-capstone-validation.Rda")

# Exploring Matrix Factorization
# See https://rafalab.github.io/dsbook/large-datasets.html#
# https://rafalab.github.io/dsbook/large-datasets.html#pca



# convert the data into a 69878x10678 users x movies matrix
# i.e. each row represents a user
train_matrix <- edx %>% 
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

# add row names
rownames(train_matrix) <- train_matrix[,1]
train_matrix <- train_matrix[,-1]

train_matrix <- sweep(train_matrix, 2, colMeans(train_matrix, na.rm=TRUE))
train_matrix <- sweep(train_matrix, 1, rowMeans(train_matrix, na.rm=TRUE))

train_matrix[is.na(train_matrix)] <- 0

image(train_matrix[1:100,1:200], xlab='Movies', ylab='Users')

system.time(pca <- prcomp(train_matrix))