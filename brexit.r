# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

brexit_polls <- 
  brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# question 2
brexit_polls %>% summarize(spread_avg=mean(spread), 
                           spread_sd=sd(spread), 
                           xhat_avg=mean(x_hat), 
                           xhat_sd = sd(x_hat))

# question 3
brexit_polls[1,]
yougov_xhat = brexit_polls$x_hat[1]
yougov_size = brexit_polls$samplesize[1]
yougov_se = sqrt(yougov_xhat * (1 - yougov_xhat) / yougov_size)
yougove_ci = yougov_xhat + c(-1, 1) * qnorm(0.975) * yougov_se

# question 4
june_polls <- brexit_polls %>%
  filter(enddate > '2016-06-01') %>%
  mutate(se_x_hat = sqrt(x_hat * (1 - x_hat) / samplesize),
         se_spread = 2 * se_x_hat,
         spread_lower = spread - qnorm(0.975) * se_spread,
         spread_upper = spread + qnorm(0.975) * se_spread,
         hit = spread_lower <= d & d <= spread_upper)
nrow(june_polls)
mean(june_polls$spread_lower < 0 & 0 < june_polls$spread_upper)
mean(june_polls$spread_lower > 0)
mean(june_polls$hit)

# question 5
june_polls %>%
  group_by(pollster) %>%
  summarize(num_polls = n(), hit_rate=mean(hit)) %>%
  arrange(desc(hit_rate))

# question 6
june_polls %>% ggplot(aes(poll_type, spread)) +
  geom_boxplot()

# question 7
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
se_spread_online <- 2 * sqrt(combined_by_type$p_hat[1] * 
                    (1 - combined_by_type$p_hat[1]) / combined_by_type$N[1])
confidence_online <- combined_by_type$spread[1] + c(-1, 1) * qnorm(0.975) * se_spread_online

# question 8
se_spread_telephone <- 2 * sqrt(combined_by_type$p_hat[2] * 
                               (1 - combined_by_type$p_hat[2]) / combined_by_type$N[2])
confidence_telephone <- combined_by_type$spread[2] + c(-1, 1) * qnorm(0.975) * se_spread_telephone
confidence_online[2] - confidence_online[1]
confidence_telephone[2] - confidence_telephone[1]

# question 9
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < d & spread_upper > d) %>%
  select(poll_type, hit)
poll_type_2by2 <- brexit_hit %>% group_by(poll_type) %>% 
  summarise(hit = sum(hit), nohit=n()-sum(hit)) %>% select(-poll_type)
poll_type_2by2 %>% chisq.test()

# question 10
brexit_polls %>% ggplot(aes(enddate, spread, color=poll_type)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_hline(yintercept=d)

# question 11
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long %>% 
  ggplot(aes(enddate, proportion, color=vote)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.3) 
  

