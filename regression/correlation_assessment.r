library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

# correlation between runs per game and bats per game
Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(R_per_game = R / G, AB_per_game = AB / G) %>%
  summarize(r=cor(R_per_game, AB_per_game)) %>%
  pull(r)

# correlation between wins per game and runs per game
Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(W_per_game = W / G, E_per_game = E / G) %>%
  summarize(r=cor(W_per_game, E_per_game))

# correlation between doubles and triples per game
Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(X2B_per_game = X2B / G, X3B_per_game = X3B / G) %>%
  summarize(r=cor(X2B_per_game, X3B_per_game)) %>%
  pull(r)