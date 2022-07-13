# Use of Recommenderlab with the full data

library("recommenderlab")
library(tidyverse)

load(file="/Users/WillMee/dev/personal/edx-datasci/capstone/data/movielens-capstone.Rda")
load(file="/Users/WillMee/dev/personal/edx-datasci/capstone/data/movielens-capstone-validation.Rda")

rmse <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# convert the data into a 69878x10678 users x movies matrix
train_matrix <- edx %>% 
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

# add row names
rownames(train_matrix) <- train_matrix[,1]
train_matrix <- train_matrix[,-1]
# 69878 x 10677 users x movies 
dim(train_matrix)

test_matrix <- validation %>% 
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

rownames(test_matrix) <- test_matrix[,1]
test_matrix <- test_matrix[,-1]
dim(test_matrix)
# 68534 x 9809 users x movies

movie_id_delta <- setdiff(unique(edx$movieId), unique(validation$movieId))

test_matrix_ext <- matrix(data=NA, nrow=dim(test_matrix)[1], ncol=length(movie_id_delta))
colnames(test_matrix_ext) <- movie_id_delta
rownames(test_matrix_ext) <- rownames(test_matrix)
test_matrix <- cbind(test_matrix, test_matrix_ext)
# ensure column order is the same in both matrices
test_matrix <- test_matrix[,colnames(train_matrix)]

# 68534 x 10677
dim(test_matrix)

# clean up
rm(test_matrix_ext)
rm(edx)
rm(validation)
rm(movie_id_delta)

# setup recommender lab data structures
train_rrm <- as(train_matrix, "realRatingMatrix")
rm(train_matrix)
test_rrm <- as(test_matrix, "realRatingMatrix")

# use RecommenderLab
rec <- Recommender(train_rrm, method = "UBCF")
rm(train_rrm)
system.time(pre <- predict(rec, test_rrm, type="ratingMatrix"))

# use pivot_longer to convert back into input for RMSE?
# https://tidyr.tidyverse.org/articles/pivot.html