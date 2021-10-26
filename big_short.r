library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)
death_prob %>% filter(age == 50 & sex == "Female")
p_death <- 0.003193
cost_death <- -150000
policy_premium <- 1150
single_expected <- p_death * cost_death + (1-p_death) * policy_premium
single_se <- abs(cost_death - policy_premium) * sqrt(p_death * (1 - p_death))
num_policies <- 1000
sum_expected <- num_policies * single_expected
sum_se <- sqrt(num_policies) * single_se
p_loose_money <- pnorm(0, sum_expected, sum_se)
p_death_male <- death_prob %>% filter(age == 50 & sex == "Male") %>% pull(prob)
policy_premium_male <- (700000 / num_policies - (p_death_male * cost_death)) / (1 - p_death_male)
sum_se_male <- sqrt(num_policies) * abs(cost_death - policy_premium_male) * sqrt(p_death_male * (1 - p_death_male))
p_loose_money_male <- pnorm(0, 700000, sum_se_male)


