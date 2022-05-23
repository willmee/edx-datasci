# Q1

library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

sum(mnist_27$train$y[indexes[1]$Resample01] == 3)

sum(indexes[1]$Resample01 == 7)
sum(unlist(indexes[1], use.names=FALSE) == 3)

sum(indexes[[1]] == 3)


# Q2
sum(sapply(seq(1,10,1), function(idx) {
  sum(indexes[[idx]]==3)
}))

# Q3
y <- rnorm(100, 0, 1)
quantile(y, 0.75)

set.seed(1, sample.kind="Rounding") # if R 3.6 or later
q75 <- replicate(10000, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

signif(mean(q75), 3)
signif(sd(q75), 3)

# Q4
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
q75 <- replicate(10, {
  bootstrap_samples <- sample(y, 100, replace = TRUE)
  quantile(bootstrap_samples, 0.75)
})
signif(mean(q75), 3)
signif(sd(q75), 3)

# Q5
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
ts_start <- Sys.time()
q75 <- replicate(10000, {
  bootstrap_samples <- sample(y, 100, replace = TRUE)
  quantile(bootstrap_samples, 0.75)
})
Sys.time() - ts_start
signif(mean(q75), 3)
signif(sd(q75), 3)



