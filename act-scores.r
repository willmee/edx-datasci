library(tidyverse)

set.seed(16)
num_scores <- 10000
scores_mean <- 20.9
scores_sd <- 5.7
act_scores <- rnorm(num_scores, scores_mean, scores_sd)

mean(act_scores)
sd(act_scores)
sum(act_scores >= 36)

sum(act_scores > 30) / 10000
sum(act_scores <= 10) / 10000

x <- 1:36
f_x <- sapply(x, dnorm, mean = 20.9, sd = 5.7)
plot(f_x, type="l")

act_scores_z = (act_scores - mean(act_scores)) / sd(act_scores)
sum(act_scores_z >= 2) / num_scores

# What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
2 * sd(act_scores) + mean(act_scores)

scores_mean
scores_sd
qnorm(0.975, scores_mean, scores_sd)

act_cdf <- function(x) {
  pnorm(x, scores_mean, scores_sd)
}
scores_pdf <- sapply(1:36, act_cdf)
min(which(scores_pdf >= 0.95))


qnorm(0.95, scores_mean, scores_sd)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
which(sample_quantiles < 26)
names(sample_quantiles[max(which(sample_quantiles < 26))])

theoretical_quantiles <- qnorm(p, scores_mean, scores_sd)

qqplot(theoretical_quantiles, sample_quantiles)
