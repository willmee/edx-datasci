library(ggplot2)
library(stringr)
library(tidyverse)
library(caret)

load(file="/Users/willmee/dev/rlang/edx-datasci/data/movielens-capstone.Rda")
load(file="/Users/willmee/dev/rlang/edx-datasci/data/movielens-capstone-validation.Rda")

# Genre effect

# dataframe with movieId and one genre per row
genres <- edx %>%
  select(movieId, genres) %>%
  distinct() %>%
  separate_rows(genres, sep='\\|') %>%
  rename(genre = genres)

# First look at a global genre effect across all users. 

# https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+2T2021/block-v1:HarvardX+PH125.8x+2T2021+type@sequential+block@7e7727ce543b4ed6ae6338626862eada/block-v1:HarvardX+PH125.8x+2T2021+type@vertical+block@4931485b1e0f43239240160db2dcf3d3
# https://rafalab.github.io/dsbook/large-datasets.html#exercises-61
# y = mu + biasUser + biasMovie + sum across genres (biasMultiplier x biasGenre) + epsilon
# where biasMultiplier = 1 if the movie is in the given genres

residuals <- edx %>%
  left_join(movie_bias, by='movieId') %>%
  left_join(user_bias, by='userId') %>%
  mutate(residual = rating - (mu + biasMovie + biasUser)) %>%
  select(userId, movieId, residual)

# use the previously calculated lambda here
# lambda <- 
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

# plot means using 95% confidence intervals
genre_bias %>%
  filter(genre != '(no genres listed)') %>%
  arrange(biasGenre) %>%
  mutate(genre = factor(genre, levels = genre)) %>%
  ggplot(aes(genre, biasGenre)) +
  geom_point(stat='identity', color='blue', alpha=0.8, aes(size=sqrt(genreCount))) +
  geom_errorbar(aes(ymin=biasGenre-(1.96*biasGenreSe), ymax=biasGenre+(1.96*biasGenreSe))) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Use a random multiplier for now
genreBiasMultiplier <- 0.5

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
      prediction = mu + mean(biasMovie) + mean(biasUser) + genreBiasMultiplier * sumBiasGenre) %>%
    pull(prediction)
)

# Prediction 6: using movie, user and genre biases with arbitrary genre
# bias multiplier, marginally less than not using the genre bias (prediction 2
# was 0.8653488)
# 0.8652524 
rmse(validation$rating, pred_6)

# Prediction 7: repeating this with arbitrary new value of multiplier
genreBiasMultiplier <- 1
# gives rmse of 0.8652952 (slightly worse)



# switch to regularization and get an optimal multiplier, reusing the previous lambda
lambda <- 5
reg_mu <- mean(edx_train$rating)
movie_bias_regularized <- edx_train %>%
  group_by(movieId) %>%
  summarize(biasMovie = sum(rating - reg_mu)/(lambda + n()))

user_bias_regularized <- edx_train %>%
  left_join(movie_bias_regularized, by='movieId') %>%
  group_by(userId) %>%
  summarize(biasUser = sum((rating - (reg_mu + biasMovie))/(lambda + n())))

genre_bias_regularized <- edx_train %>%
  left_join(movie_bias_regularized, by='movieId') %>%
  left_join(user_bias_regularized, by='userId') %>%
  separate_rows(genres, sep='\\|') %>%
  rename(genre = genres) %>%
  left_join(genres, by = c('movieId', 'genre')) %>%
  group_by(genre) %>%
  summarize(
    biasGenre = sum((rating - (reg_mu + biasMovie + biasUser))/(lambda + n())),
    genreCount = n()
  )

genre_bias_regularized <- genre_bias_regularized %>%
  filter(genreCount > 100000)


multiplier <- 0.5
regularized_prediction_movie_user_genre <- function(multiplier) {
  pred <- edx_test %>%
    separate_rows(genres, sep='\\|') %>%
    rename(genre = genres) %>%
    left_join(movie_bias_regularized, by='movieId') %>%
    left_join(user_bias_regularized, by='userId') %>%
    left_join(genre_bias_regularized, by='genre') %>%
    mutate(biasGenre = ifelse(is.na(biasGenre), 0, biasGenre)) %>%
    group_by(userId, movieId) %>%
    summarize(prediction = reg_mu + mean(biasMovie) + mean(biasUser) + (multiplier * sum(biasGenre))/n()) %>%
    pull(prediction)

  rmse(edx_test$rating, pred)
}

genre_multipliers <- seq(1.5, 3, 0.1)
rmses_regularized_movie_user_genre_prediction <- sapply(genre_multipliers, function(x) {
  regularized_prediction_movie_user_genre(x)
})
plot(genre_multipliers, rmses_regularized_movie_user_genre_prediction)
genre_multipliers[which.min(rmses_regularized_movie_user_prediction)]
# 2.2 is the minimum genre multiplier.

# Now redo the above on the full training dataset and evaluate against the full
# testing one, using the calculcated multiplier
mu_hat <- mean(edx$rating)
lamda <- 5
genre_multiplier <- 2.2

movie_bias_regularized_full <- edx %>%
  group_by(movieId) %>%
  summarize(biasMovie = sum(rating - mu_hat)/(lambda + n()))

user_bias_regularized_full <- edx %>%
  left_join(movie_bias_regularized_full, by='movieId') %>%
  group_by(userId) %>%
  summarize(biasUser = sum((rating - (mu_hat + biasMovie))/(lambda + n())))

genre_bias_regularized_full <- edx %>%
  left_join(movie_bias_regularized_full, by='movieId') %>%
  left_join(user_bias_regularized_full, by='userId') %>%
  separate_rows(genres, sep='\\|') %>%
  rename(genre = genres) %>%
  left_join(genres, by = c('movieId', 'genre')) %>%
  group_by(genre) %>%
  summarize(
    biasGenre = sum((rating - (mu_hat + biasMovie + biasUser))/(lambda + n())),
    genreCount = n()
  )  %>%
  filter(genreCount > 100000)

pred_8 <- validation %>%
    separate_rows(genres, sep='\\|') %>%
    rename(genre = genres) %>%
    left_join(movie_bias_regularized_full, by='movieId') %>%
    left_join(user_bias_regularized_full, by='userId') %>%
    left_join(genre_bias_regularized_full, by='genre') %>%
    mutate(biasGenre = ifelse(is.na(biasGenre), 0, biasGenre)) %>%
    group_by(userId, movieId) %>%
    summarize(prediction = mu_hat + mean(biasMovie) + mean(biasUser) + (genre_multiplier * sum(biasGenre))/n()) %>%
    pull(prediction)
  
# prediction using genre bias and regularization
# 0.8647045 (which is slightly less than prediction 5 which doesn't use genre)
rmse(validation$rating, pred_8)


