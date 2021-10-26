library(tidyverse)
library(dslabs)
data(death_prob)
p_death <- 0.015
cost_death <- -150000
policy_premium <- 1150
single_expected <- p_death * cost_death + (1-p_death) * policy_premium
single_se <- abs(cost_death - policy_premium) * sqrt(p_death * (1 - p_death))
num_policies <- 1000
sum_expected <- num_policies * single_expected
sum_se <- sqrt(num_policies) * single_se
p_loose_money <- pnorm(0, sum_expected, sum_se)

p_loose_million <- pnorm(-1000000, sum_expected, sum_se)

p <- seq(.01, .03, .001)
prob_loose_money <- sapply(p, function(p_death) {
  single_expected <- p_death * cost_death + (1-p_death) * policy_premium
  single_se <- abs(cost_death - policy_premium) * sqrt(p_death * (1 - p_death))
  sum_expected <- num_policies * single_expected
  sum_se <- sqrt(num_policies) * single_se
  p_loose_money <- pnorm(0, sum_expected, sum_se)
})

p[which.max(prob_loose_money > 0.9)]

p_new <- seq(.01, .03, .0025)
prob_loose_million <- sapply(p_new, function(p_death) {
  single_expected <- p_death * cost_death + (1-p_death) * policy_premium
  single_se <- abs(cost_death - policy_premium) * sqrt(p_death * (1 - p_death))
  sum_expected <- num_policies * single_expected
  sum_se <- sqrt(num_policies) * single_se
  p_loose_money <- pnorm(-1000000, sum_expected, sum_se)
})
p_new[which.max(prob_loose_million > 0.9)]

set.seed(25, sample.kind = "Rounding")
p_loss = .015
sum(sample(c(policy_premium, cost_death), num_policies, replace=TRUE, prob=c(1-p_loss, p_loss)))/1000000

set.seed(27, sample.kind = "Rounding")
B <- 10000
loose_million <- replicate(B, {
  sum(sample(c(policy_premium, cost_death), num_policies, replace=TRUE, prob=c(1-p_loss, p_loss)))
})
mean(loose_million < -1000000)
