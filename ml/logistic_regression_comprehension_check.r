library(tidyverse)
library(ggplot2)

# set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

# visualize the data
# dat$train %>% ggplot(aes(x, color = y)) + geom_density()

log_regression_accuracy <- function(mu)  {
  dat = make_data(mu_1 = mu)
  glm_fit <- glm(y ~ x, data=dat$train, family = "binomial")
  p_hat_logit <- predict(glm_fit, newdata = dat$test, type = "response")
  y_hat <- ifelse(p_hat_logit > 0.5, 1, 0)
  mean(y_hat == dat$test$y)
}

mu_1 <- seq(0, 3, len=25)
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
accuracy_dat <- data.frame(mu_1 = mu_1, accuracy = sapply(mu_1, log_regression_accuracy))
ggplot(accuracy_dat, aes(x=mu_1, y=accuracy)) + geom_point()