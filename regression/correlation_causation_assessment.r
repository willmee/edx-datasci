library(tidyverse)

# Question 1
# In the videos, we ran one million tests of correlation for two random variables, X and Y.
# How many of these correlations would you expect to have a significant p-value (), just by chance?
  
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))
g_small <- 1000
sim_data_small <- tibble(group = rep(1:g_small, each = N), x = rnorm(N * g_small), y = rnorm(N * g_small))

sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(y ~ x, data = .)))

sim_data_lm <- sim_data_small %>% 
  group_by(group) %>%
  do(tidy(lm(y ~ x, data = .))) 

sim_data_lm %>%
  filter(term == 'x') %>%
  ggplot(aes(x=p.value)) + geom_histogram(binwidth = 0.05, color = "black")

qnorm(0.05)
pnorm(0.05)

N <- 25
g <- 1000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))
sim_data %>%
  group_by(group) %>%
  do(tidy(lm(y ~ x, data = .))) %>%
  filter(term == 'x') %>%
  ggplot(aes(x=p.value)) + geom_histogram(binwidth = 0.05, color = "black")
  
