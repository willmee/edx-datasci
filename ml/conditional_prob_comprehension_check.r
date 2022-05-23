# Q1
# P(disease|test+) = P(test+|disease) * P(disease)/P(test+)
# Don't have P(test+)
#
#              Reality
# Test    disease   healthy
# test+   TP        FP
# test-   FN        TN
# P(test+) = TP + FP / (TP + FP + FN + TN)
# sensitivity:
# TP / (TP + FN) = 0.85
# specificity
# TN / (TN + FP) = 0.90
# prevalence:
# TP + FN / (TP + FP + FN + TN) = 0.02

# Fix TP = 1000, then
# 1000 + FN = 1000 / 0.85 
# FN = (1000 / 0.85) - 1000
# FN = 176
#
# TP + FN / (TP + FP + FN + TN) = 0.02
# 1000 + 176 / (1000 + FP + 156 + TN) = 0.02
# 1176 / (1176 + TN + FP) = 0.02
# TN + FP + 1176 = 1176 / 0.02 = 58800
# TN + FP = 57624
#
# TN = 0.90 (TN + FP)
# TN = 0.90 * 57624 = 51861.6
#
# FP = 57624 - 51861.6 = 5762.4

#  P(test+) = TP + FP / (TP + FP + FN + TN)
#           = (1000 + 5762.4) / (1000 + 176 + 51861.6 + 5762.4)
#           = 6762.4 / 58800
#           = 0.115
#
# Or somewhat simpler:
# ? = P(test+|disease)*P(disease) + P(test+|healthy)*P(healthy)
#          = 0.85 * 0.02 + (1-P(test-|healthy)) * (1-P(disease))
#          = 0.017 + (0.1 * 0.98) = 0.017 + 0.098
#          = 0.115
0.85 * 0.02/0.115
# 0.148


# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# Q2
# What is the probability that a test is positive?
# P(test+) = P(test+|disease)*P(disease) + P(test+|healthy)*P(healthy)
#          = 0.85 * 0.02 + (1-P(test-|healthy)) * (1-P(disease))
#          = 0.017 + (0.1 * 0.98) = 0.017 + 0.098
#          = 0.115
mean(test == 1)

# Q3
# What is the probability that an individual has the disease if the test is negative?
# P(disease|test-) = P(test-|disease) * P(disease)/P(test-)
#                  = (1 - P(test+|disease)) * 0.02/(1 - P(test+))
#                  = (1 - 0.85) * 0.02 / (1 - 0.115)
#                  = 0.15 * 0.02 / 0.885
mean(disease[test == 0])

# Q4
# What is the probability that you have the disease if the test is positive?
# P(disease|test+)
# same as Q1
mean(disease[test==1])

# Q5
# Compare the prevalence of disease in people who test positive to the overall prevalence of disease.
# If a patient's test is positive, by how many times does that increase their risk of having the disease?

# First calculate the probability of having the disease given a positive test, then divide by the probability of having the disease.
# P(disease|test+)/P(disease) = 0.148/0.02
mean(disease[test==1])/mean(disease)

library(dslabs)
data("heights")

# Q6
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

# Q7
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

# Q8
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

# Q9
ps <- seq(0, 1, 0.1)
dat %>% 
  # MISSING CODE
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)