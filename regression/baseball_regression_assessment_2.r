library(Lahman)

# linear regression with two variables
fit <- Teams %>% 
  filter(yearID == 1971) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)


Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  filter(term == 'BB') %>%
  ggplot(aes(yearID, estimate)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

fit_orig <- Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  #mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  filter(term == 'BB') %>%
  #ungroup() %>%
  lm(estimate ~ yearID, data = .)
tidy(fit_orig)  


res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 

fit <- res %>%
  filter(term == 'BB') %>%
  lm(estimate ~ yearID, data = .)
tidy(fit)