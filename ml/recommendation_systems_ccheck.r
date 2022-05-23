library(tidyverse)
library(lubridate)
library(dslabs)
library(plyr)
library(ggplot2)
data("movielens")

# Q1
rating_counts <- movielens %>%
  group_by(movieId) %>%
  summarize(ratingCount = n(), year=max(year))

rating_counts %>%
  ggplot(aes(x=year, y=sqrt(ratingCount), group=year)) +
  geom_boxplot()

# Q2
movielens %>%
  group_by(movieId) %>%
  summarize(year=first(year), title=first(title), ratingCount = n(), ratingAvg = mean(rating)) %>%
  filter(year >= 1993) %>%
  mutate(ratingsPerYear = ratingCount/(2018-year)) %>%
  slice_max(ratingsPerYear, n=25) 

# Q3
rating_counts_per_year <- movielens %>%
  group_by(movieId) %>%
  summarize(year=first(year), title=first(title), ratingCount = n(), ratingAvg = mean(rating)) %>%
  filter(year >= 1993) %>%
  mutate(ratingsPerYear = ratingCount/(2018-year)) 

rating_counts_per_year %>%
  ggplot(aes(ratingsPerYear, ratingAvg)) +
  geom_point(colour = "black", aes(alpha=0.9)) +
  geom_smooth(method=lm, colour="red")
  
# Q5
movielens %>% 
  mutate(date = as_datetime(timestamp)) %>% head

# Q6
weekly_avg <- movielens %>% 
  mutate(date = round_date(as_datetime(timestamp), unit="week")) %>% 
  group_by(date) %>%
  summarize(weeklyAvgRating=mean(rating)) 
  
weekly_avg %>% 
  ggplot(aes(date, weeklyAvgRating)) + 
  geom_point() +
  geom_smooth(method=lm, colour="red")

# Q6
top_genres <- movielens %>%
  group_by(genres) %>%
  summarize(ratingAvg = mean(rating), error=sd(rating), ratingCount=n()) %>%
  filter(ratingCount > 1000)

top_genres %>%
  left_join(movielens, by="genres") %>%
  boxplot(rating~genres, data = .)

# model answer
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
