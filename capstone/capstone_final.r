library(tidyverse)
library(caret)
library(ggplot2)
library(recommenderlab)

# I cached the extracted dataset - these lines should be commented out in
# final assessment.
load(file="/Users/WillMee/dev/personal/edx-datasci/capstone/data/movielens-capstone.Rda")
load(file="/Users/WillMee/dev/personal/edx-datasci/capstone/data/movielens-capstone-validation.Rda")

rmse <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# 0. Baseline
mu_hat <- mean(edx$rating)
pred_0 <- rep(mu_hat, nrow(validation))
rmse(validation$rating, pred_0)

# 1. Movie Bias
movie_bias <- edx %>%
  group_by(movieId) %>%
  summarize(biasMovie = mean(rating)-mu_hat)

pred_1 <- validation %>%
  left_join(movie_bias, by='movieId') %>%
  pull(biasMovie)

pred_1 <- pred_1 + mu_hat
rmse(validation$rating, pred_1)

# 2. Movie/User Bias
user_bias <- edx %>%
  left_join(movie_bias, by='movieId') %>%
  group_by(userId) %>%
  summarize(biasUser = mean(rating - mu_hat - biasMovie)) 

pred_2 <- validation %>%
  left_join(movie_bias, by='movieId') %>%
  left_join(user_bias, by='userId') %>%
  mutate(prediction = mu_hat + biasMovie + biasUser) %>%
  pull(prediction)

# Prediction 2: 0.8653488
rmse(validation$rating, pred_2)

# Regularization
set.seed(51, sample.kind='Rounding')
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
temp <- edx[test_index,]

edx_test <- temp %>% 
  semi_join(edx_train, by = 'movieId') %>%
  semi_join(edx_train, by = 'userId')

removed <- anti_join(temp, edx_test, by=c('movieId', 'userId'))
edx_train <- rbind(edx_train, removed)

# 3. Movie Bias reg
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

lambda <- lambdas[which.min(rmses_regularized_movie)]

# Prediction 3: regularization, just movie 
# 0.9438542
regularized_prediction_movie(lambda, edx, validation)

# 4. Movie/User Bias reg
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

lambdas <- seq(3, 10, 0.1)
rmses_regularized_movie_user <- sapply(lambdas, function(x) {
  regularized_prediction_movie_user(x, edx_train, edx_test)
})

lambdas_df <- data.frame(lambda=lambdas, rmse=rmses_regularized_movie_user)
ggplot(data=lambdas_df, aes(x=lambda, y=rmse)) +
  geom_line(color='blue') +
  geom_point(color='blue') +
  ggtitle('Effect of varying lambda on regularized RMSE for User/Group Model') +
  xlab('Lambda parameter') +
  ylab('Regularized RMSE')

lambda <- lambdas[which.min(rmses_regularized_movie_user)]
lambda

# Prediction 4: regularization with movie and user and optimal lambda
regularized_prediction_movie_user(lambda, edx, validation)

# 5. Genres
genres <- edx %>%
  select(movieId, genres) %>%
  distinct() %>%
  separate_rows(genres, sep='\\|') %>%
  rename(genre = genres)

residuals <- edx %>%
  left_join(movie_bias, by='movieId') %>%
  left_join(user_bias, by='userId') %>%
  mutate(residual = rating - (mu_hat + biasMovie + biasUser)) %>%
  select(userId, movieId, residual)

genre_bias <- residuals %>%
  left_join(genres, by = 'movieId') %>%
  group_by(genre) %>%
  summarize(
    biasGenre = mean(residual), 
    biasGenreSd = sd(residual),
    biasGenreSe = sd(residual)/sqrt(n()),
    genreCount = n(),
    biasGenreReg = sum(residual)/(genreCount + lambda)
  ) %>%
  filter(genreCount>=100000)

# genre bias plot
genre_bias %>%
  filter(genre != '(no genres listed)') %>%
  arrange(biasGenre) %>%
  mutate(genre = factor(genre, levels = genre)) %>%
  ggplot(aes(genre, biasGenre)) +
  geom_point(stat='identity', color='blue', alpha=0.8, aes(size=sqrt(genreCount))) +
  geom_errorbar(aes(ymin = biasGenre - (1.96 * biasGenreSe), ymax = biasGenre + (1.96 * biasGenreSe))) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Genre") +
  ylab("Genre bias") +
  labs(size = "Review count (sqrt)")

# Omitting code to calculate this, which is a variation of the code above
genreBiasMultiplier <- 2.2

# measure time, because this is surprisingly slow - 46 seconds
system.time(
  pred_6 <- validation %>%
    left_join(movie_bias, by='movieId') %>%
    left_join(user_bias, by='userId') %>%
    separate_rows(genres, sep='\\|') %>%
    rename(genre = genres) %>%
    left_join(genre_bias, by='genre') %>%
    group_by(userId, movieId) %>%
    summarize(
      # need to handle cases in which the genre is missing for the movie
      sumBiasGenre = sum(biasGenre), 
      sumBiasGenre = ifelse(is.na(sumBiasGenre), 0, sumBiasGenre),
      prediction = mu_hat + mean(biasMovie) + mean(biasUser) + genreBiasMultiplier * sumBiasGenre) %>%
    pull(prediction)
)
rmse(validation$rating, pred_6)

# matrix conversion
train_matrix <- edx %>% 
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = 'movieId', values_from = 'rating') %>%
  as.matrix()

# add row names
rownames(train_matrix) <- train_matrix[,1]
train_matrix <- train_matrix[,-1]
# 69878 x 10677 users x movies 
dim(train_matrix)


test_matrix <- validation %>% 
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = 'movieId', values_from = 'rating') %>%
  as.matrix()

rownames(test_matrix) <- test_matrix[,1]
test_matrix <- test_matrix[,-1]
# 68534 x 9809 users x movies
dim(test_matrix)

movie_id_delta <- setdiff(unique(edx$movieId), unique(validation$movieId))

test_matrix_ext <- matrix(data=NA, nrow=dim(test_matrix)[1], ncol=length(movie_id_delta))
colnames(test_matrix_ext) <- movie_id_delta
rownames(test_matrix_ext) <- rownames(test_matrix)
test_matrix <- cbind(test_matrix, test_matrix_ext)
# ensure column order is the same in both matrices
test_matrix <- test_matrix[,colnames(train_matrix)]

# 68534 x 10677
dim(test_matrix)

# matrix-specific functions
matrix_sparcity <- function(matrix) {
  matrix_dim <- dim(matrix)  
  sum(is.na(matrix))/(matrix_dim[1] * matrix_dim[2])
}

rmse_matrix <- function(actual, predicted) {
  stopifnot(dim(actual) == dim(predicted))
  actual_cp <- actual
  predicted_cp <- predicted
  nas <- is.na(actual)
  predicted_cp[nas] <- -1
  residual_na_ratio <- matrix_sparcity(predicted_cp)
  actual_cp[nas] <- -1
  residual_nas <- is.na(predicted_cp)
  predicted_cp[residual_nas] <- -2
  actual_cp[residual_nas] <- -2
  list(rmse = sqrt(sum((actual_cp - predicted_cp)^2)/(sum(!(nas | residual_nas)))), na_ratio=residual_na_ratio)
}

# Recommenderlab
train_rrm <- as(train_matrix, 'realRatingMatrix')
test_rrm <- as(test_matrix, 'realRatingMatrix')

rec_ubcf <- Recommender(train_rrm, method = 'UBCF')
rec_random <- Recommender(train_rrm, method = 'RANDOM')
rec_svd <- Recommender(train_rrm, method = 'SVD')
rec_svdf <- Recommender(train_rrm, method = 'SVDF')
rec_ibcf <- Recommender(train_rrm, method = 'IBCF')

# SVD
system.time(pre_svd <- predict(rec_svd, test_rrm, type="ratingMatrix"))
rmse_matrix(test_matrix, as(pre_svd, "matrix"))

# IBCF
system.time(pre_ibcf <- predict(rec_ibcf, test_rrm, type="ratingMatrix"))
rmse_matrix(test_matrix, as(pre_ibcf, "matrix"))

# Final comparison
model_results <- data.frame(
  models=c(
    "Baseline",
    "Movie Bias", 
    "User/Movie Bias", 
    "Movie Bias Reg", 
    "User/Movie Bias Reg", 
    "Movie/User/Genre Bias Reg", 
    "SVD", 
    "ICBF"),
  rmse=c(
    1.061202,
    0.9439087,
    0.8653488,
    0.9438542,
    0.8648177,
    0.8647045,
    0.8356373,
    0.9867809)
) %>%
  arrange(desc(rmse))

model_results$models <- factor(model_results$models, levels = model_results$models)

ggplot(model_results, aes(x = models, y = rmse)) +
  geom_segment(aes(x = models, xend = models, y = 0.6, yend = rmse),
               color = "blue", lwd = 1, linetype = "dotted") +
  geom_point(size = 5, pch = 21, bg = 4, col = 1) +
  ylim(0.6, 1.1) +
  xlab('Model') +
  ylab('RMSE') +
  coord_flip() +
  theme_minimal()

