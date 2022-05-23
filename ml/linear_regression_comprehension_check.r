library(tidyverse)
library(caret)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# Q1
# replicate() loop, 
# (1) partition the dataset into test and training sets with p = 0.5 and using dat$y to generate your indices, 
# (2) train a linear model predicting y from x, 
# (3) generate predictions on the test set, and 
# (4) calculate the RMSE of that model. 
# Then, report the mean and standard deviation (SD) of the RMSEs from all 100 models.
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

rmses <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  test_set <- dat[test_index, ]
  train_set <- dat[-test_index, ]
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(rmses)
sd(rmses)

# Q2

rmse_stats <- function(data_size) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = data_size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmses <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    test_set <- dat[test_index, ]
    train_set <- dat[-test_index, ]
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  c(signif(mean(rmses), 3), signif(sd(rmses), 3))
}

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- c(100, 500, 1000, 5000, 10000)
sapply(n, rmse_stats)

# Q3

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
rmses <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  test_set <- dat[test_index, ]
  train_set <- dat[-test_index, ]
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

signif(mean(rmses), 3)
signif(sd(rmses), 3)

# Q6, Q7
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]
fit_1 <- lm(y ~ x_1, data = train_set)
fit_2 <- lm(y ~ x_2, data = train_set)
fit_both <- lm(y ~ x_1 + x_2, data = train_set)
y_hat_1 <- predict(fit_1, test_set)
y_hat_2 <- predict(fit_2, test_set)
y_hat_both <- predict(fit_both, test_set)

signif(sqrt(mean((y_hat_1 - test_set$y)^2)), 3)
signif(sqrt(mean((y_hat_2 - test_set$y)^2)), 3)
signif(sqrt(mean((y_hat_both - test_set$y)^2)), 3)

# Q8
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]
fit_1 <- lm(y ~ x_1, data = train_set)
fit_2 <- lm(y ~ x_2, data = train_set)
fit_both <- lm(y ~ x_1 + x_2, data = train_set)
y_hat_1 <- predict(fit_1, test_set)
y_hat_2 <- predict(fit_2, test_set)
y_hat_both <- predict(fit_both, test_set)

signif(sqrt(mean((y_hat_1 - test_set$y)^2)), 3)
signif(sqrt(mean((y_hat_2 - test_set$y)^2)), 3)
signif(sqrt(mean((y_hat_both - test_set$y)^2)), 3)