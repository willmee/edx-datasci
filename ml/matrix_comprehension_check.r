# Q1
x <- matrix(rnorm(100*10), 100, 10)

# Q5
rowMeans(x)
colMeans(x)

# Q6
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
class(mnist$train$images)
head(mnist$train$images)
grey_pixels <- (mnist$train$images > 50 & mnist$train$images < 205)
signif(mean(as.vector(grey_pixels)), digits=3)