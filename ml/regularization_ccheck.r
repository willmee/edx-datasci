library(ggplot2)

options(digits=7)

# data construction
# 1. school size
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

# 2. independent real quality
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))


schools %>% top_n(10, quality) %>% arrange(desc(quality))


# test results 
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

# Q1
top_scoring_schools <- head(schools[order(-schools$score),], n=10)
# or
schools %>% top_n(10, score) %>% arrange(desc(score))
top_scoring_schools

# Q2
median(schools$size)
# 185.5
top_scoring_schools %>% summarize(median_size=median(size))

# Q3
bottom_scoring_schools <- schools %>% top_n(10, -score) %>% arrange(score)
bottom_scoring_schools
bottom_scoring_schools %>% summarize(median_size=median(size))

# Q4
schools %>% top_n(10, quality)
schools %>%
  arrange(desc(quality)) %>% 
  mutate(top_quality=row_number()<=10) %>%
  ggplot(aes(x=size, y=score)) +
  geom_point(aes(color=top_quality), alpha=0.5)

# model answer
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)

# Q5
overall <- mean(sapply(scores, mean))
alpha <- 25

reglarization <- sapply(scores, function(x) {
  sum(x > overall)/(alpha + length(x))
})

schools_regularized <- schools %>% 
  mutate(regularized = sapply(scores, function(x) {
    overall + sum(x - overall)/(alpha + length(x))
  }))

schools_regularized %>% top_n(10, regularized) %>% arrange(-regularized)

# model answer
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
# this is just the mean
score_unreg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)))

schools_reg <- schools %>% mutate(score_reg = score_reg, score_unreg=score_unreg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))
# 203.5
schools_reg %>% 
  top_n(10, score_reg) %>% 
  arrange(desc(score_reg)) %>%
  summarize(median_size=median(size))

# Q6
rmse <- function(alpha) {
  score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
  sqrt(sum((schools$quality - score_reg)^2)/nrow(schools))
}
rmse(25)
rmse_data <- sapply(seq(10, 250), rmse)
rmse_df <- data.frame(alpha = seq(10, 250),
                      rmse = rmse_data)
rmse_df %>% ggplot(aes(alpha, rmse)) + geom_point()
rmse_df[which.min(rmse_df$rmse),]

# Q7
alpha_best = 135
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha_best))
schools_reg_best <- schools %>% 
  mutate(score_reg = score_reg)

schools_reg_best %>% top_n(10, score_reg) %>% arrange(desc(score_reg))

# Q8
rmse_degenerate <- function(alpha) {
  score_reg <- sapply(scores, function(x)  sum(x)/(length(x)+alpha))
  sqrt(sum((schools$quality - score_reg)^2)/nrow(schools))
}
alphas <- seq(10, 250)
rmse_degen_df <- data.frame(alpha = alphas,
                      rmse = sapply(alphas, rmse_degenerate))
rmse_degen_df %>% ggplot(aes(alpha, rmse)) + geom_point()

