library(tidyverse)
library(HistData)
library(Lahman)
library(dslabs)
data("GaltonFamilies")
ds_theme_set()
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)


# Assessment: Least Squares Estimates, part 1
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

# lm with two predictor values
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R/G, HR_per_game = HR/G, BB_per_game = BB/G) %>%
  lm(R_per_game ~ BB_per_game + HR_per_game, data = .)

B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
head(lse)

# Assessment: Least Squares Estimates, part 2

# Fit a linear regression model predicting the mothers' heights using daughters' heights. 
female_heights %>% lm(mother ~ daughter, data = .) 

# Question 8: Predict mothers' heights using the model. 
mdcoeff <- female_heights %>% lm(mother ~ daughter, data = .)  %>% .$coef
mdcoeff <- as.vector(unlist(mdcoeff, use.names=FALSE))
md_intercept = mdcoeff[1]
md_grad = mdcoeff[2]
head(female_heights)
(female_heights[1, 'daughter'] * md_grad) + md_intercept

# Question 9
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_03 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) %>% group_by(playerID) %>%
  mutate(mean_singles=mean(singles), mean_bb=mean(bb), n_rows=n()) %>% 
  sample_n(1) %>%
  ungroup() %>%
  select(!c(singles,bb))

sum(bat_03$mean_singles > 0.2)
sum(bat_03$mean_bb > 0.2)

# Question 10
bat_02 %>%
  inner_join(bat_03,by="playerID") %>%
  summarize(r = cor(singles, mean_singles))

bat_02 %>%
  inner_join(bat_03,by="playerID") %>%
  summarize(r = cor(bb, mean_bb))

# Question 11

# simple scatter plot
bat_02 %>%
  inner_join(bat_03,by="playerID") %>%
  ggplot(aes(bb, mean_bb)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

# Scatter plots statified by means
bat_02 %>%
  inner_join(bat_03,by="playerID") %>%
  mutate(mean_bb_strata = round(mean_bb, digits=2)) %>%
  group_by(mean_bb_strata) %>%
  filter(n() > 5) %>%
  ungroup() %>%
  ggplot(aes(bb, mean_bb)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ mean_bb_strata) 

bat_02 %>%
  inner_join(bat_03,by="playerID") %>%
  mutate(mean_singles_strata = round(mean_singles, digits=2)) %>%
  group_by(mean_singles_strata) %>%
  filter(n() > 5) %>%
  ungroup() %>%
  ggplot(aes(singles, mean_singles)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ mean_singles_strata) 

# Question 12
bat_joined <- bat_02 %>%
  inner_join(bat_03,by="playerID")

bat_joined %>% lm(singles ~ mean_singles, data=.)

bat_joined %>% lm(bb ~ mean_bb, data=.)