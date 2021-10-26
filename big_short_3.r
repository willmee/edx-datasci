library(tidyverse)
library(dslabs)


p_death <- 0.015
cost_death <- -150000
num_policies <- 1000
l <- cost_death
z <- qnorm(0.05)
n <- num_policies
p <- p_death
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))

profit_per_policy <- cost_death * p_death + x * (1 - p_death)

set.seed(28, sample.kind = "Rounding")
B <- 10000
money_earned <- replicate(B, {
  sum(sample(c(cost_death, x), num_policies, replace = TRUE, prob=c(p_death, 1-p_death)))
})
mean(money_earned < 0)

set.seed(29, sample.kind = "Rounding")
changed_money_earned <- replicate(B, {
  changed_p_death <- p_death + sample(seq(-0.01, 0.01, length = 100), 1)
  sum(sample(c(cost_death, x), num_policies, replace = TRUE, prob=c(changed_p_death, 1-changed_p_death)))
})
mean(changed_money_earned)
