set.seed(12, sample.kind = "Rounding")

S <- replicate(10000, {
  sum(sample(c(1, -0.25), 44, replace=TRUE, prob=c(0.2, 0.8)))
})

p <- seq(0.25, 0.95, 0.05)
sat_questions <- 44
# What is the lowest p such that the probability of scoring over 35 exceeds 80%?
results <- sapply(p, function(prob) {
  average <- sat_questions * prob
  stdv <- sqrt(sat_questions) * 1 * sqrt(prob * (1-prob))
  1 - pnorm(35, average, stdv)
})

which.min(results > 0.8)