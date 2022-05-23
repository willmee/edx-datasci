# See recommendation systems
# https://rafalab.github.io/dsbook/large-datasets.html#recommendation-systems
# 
# Also the Netflix challenge:
# http://blog.echen.me/2011/10/24/winning-the-netflix-prize-a-summary/
# http://www.netflixprize.com/assets/GrandPrize2009_BPC_BellKor.pdf

library(tidyverse)
library(caret)
library(data.table)

# root mean square error
rmse <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# simple model of means of all ratings
pred_1 <- edx %>%
  group_by(movieId) %>%
  summarize(prediction = mean(rating))

y_hat_1 = pred_1$prediction

names(y_hat_1) <- as.character(pred_1$movieId)
y_hat_1[2]

valid_1 <- validation %>%
  mutate(prediction = y_hat_1[movieId]) %>%
  select(movieId, rating, prediction)

rmse(head(valid_1$rating), head(as.vector(valid_1$prediction)))
