# See recommendation systems
# https://rafalab.github.io/dsbook/large-datasets.html#recommendation-systems
# 
# Also the Netflix challenge:
# http://blog.echen.me/2011/10/24/winning-the-netflix-prize-a-summary/
# http://www.netflixprize.com/assets/GrandPrize2009_BPC_BellKor.pdf

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)

# root mean square error
rmse <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# zero-model: just use the mean
mu_hat <- mean(edx$rating)
# 3.512465
mu_hat

pred_0 <- rep(mu_hat, nrow(validation))

# Prediction 0 (Baseline) is 1.061202
rmse(validation$rating, pred_0)

# simple model of means of all ratings per movie 
mu <- mean(edx$rating)
movie_bias <- edx %>%
  group_by(movieId) %>%
  summarize(biasMovie = mean(rating)-mu)

pred_1 <- validation %>%
  left_join(movie_bias, by='movieId') %>%
  pull(biasMovie)

pred_1 <- pred_1 + mu

# Prediction 1: 0.9439087
rmse(validation$rating, pred_1)

# User bias model
user_bias <- edx %>%
  left_join(movie_bias, by='movieId') %>%
  group_by(userId) %>%
  summarize(biasUser = mean(rating-mu-biasMovie)) 

pred_2 <- validation %>%
  left_join(movie_bias, by='movieId') %>%
  left_join(user_bias, by='userId') %>%
  mutate(prediction = mu + biasMovie + biasUser) %>%
  pull(prediction)

# Prediction 2: 0.8653488
rmse(validation$rating, pred_2)

# regularization - just movie

# get a good value of the lambda parameter from the test data only, using the
# same approach as the main edx dataframe
set.seed(51, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
temp <- edx[test_index,]

edx_test <- temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)

regularized_prediction_movie <- function(lambda, train, test) {
  reg_mu <- mean(train$rating)
  movie_bias_regularized <- train %>%
    group_by(movieId) %>%
    summarize(biasMovie = sum(rating - reg_mu)/(lambda + n()))
  
  pred <- reg_mu + test %>%
    left_join(movie_bias_regularized, by='movieId') %>%
    pull(biasMovie)
  
  rmse(test$rating, pred)
}

lambdas <- seq(0, 5, 0.25)
rmses_regularized_movie <- sapply(lambdas, function(x) {
  regularized_prediction_movie(x, edx_train, edx_test)
  }
)
plot(lambdas, rmses_regularized_movie)
lambda <- lambdas[which.min(rmses_regularized_movie)]
# Prediction 3: regularization, just movie 
# 0.9438542
regularized_prediction_movie(lambda, edx, validation)

# regularization with movie and user
regularized_prediction_movie_user <- function(lambda, train, test) {
  reg_mu <- mean(train$rating)

  movie_bias_regularized <- train %>%
    group_by(movieId) %>%
    summarize(biasMovie = sum(rating - reg_mu)/(lambda + n()))
  
  user_bias_regularized <- train %>%
    left_join(movie_bias_regularized, by='movieId') %>%
    group_by(userId) %>%
    summarize(biasUser = sum(rating - reg_mu - biasMovie)/(lambda + n())) 
  
  pred <- test %>%
    left_join(movie_bias_regularized, by='movieId') %>%
    left_join(user_bias_regularized, by='userId') %>%
    mutate(pred = reg_mu + biasUser + biasMovie) %>%
    pull(pred)
  
  rmse(test$rating, pred)
}

# Prediction 4: regularization with movie and user with arbitrary lambda
# 0.8650119
regularized_prediction_movie_user(lambda, edx, validation)

lambdas <- seq(3, 10, 0.1)
rmses_regularized_movie_user <- sapply(lambdas, function(x) {
  regularized_prediction_movie_user(x, edx_train, edx_test)
}
)
plot(lambdas, rmses_regularized_movie_user)

lambdas_df <- data.frame(lambda=lambdas, rmse=rmses_regularized_movie_user)
ggplot(data=lambdas_df, aes(x=lambda, y=rmse)) +
  geom_line(color='blue') +
  geom_point(color='blue') +
  ggtitle('Effect of varying lambda on regularized RMSE for User/Group Model') +
  xlab('Lambda parameter') +
  ylab('Regularized RMSE')

lambda <- lambdas[which.min(rmses_regularized_movie_user)]
# optimal lambda is 5.0

# Prediction 5: regularization with movie and user and optimal lambda
# 0.8648177
regularized_prediction_movie_user(lambda, edx, validation)
