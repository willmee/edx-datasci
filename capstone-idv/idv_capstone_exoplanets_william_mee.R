# Requires registration at Kaggle - not a public dataset.
# https://www.kaggle.com/datasets/keplersmachines/kepler-labelled-time-series-data

library(ggplot2)
library(tidyverse)

# update as needed
setwd('/Users/willmee/dev/rlang/edx-datasci/capstone-idv')
data_dir <- paste(getwd(), 'data', sep='/')

# read in the data set
kepler_train <- read.csv(file = paste(data_dir, 'exoTrain.csv', sep='/'))
kepler_test <- read.csv(file = paste(data_dir, 'exoTest.csv', sep='/'))

kepler_train$LABEL <- as.factor(kepler_train$LABEL)
kepler_test$LABEL <- as.factor(kepler_test$LABEL)
names(kepler_train)[names(kepler_train) == 'LABEL'] <- 'category'

# massage to long-form
kepler_train$star <- seq(1, nrow(kepler_train))
kepler_train <- kepler_train %>% 
  pivot_longer('FLUX.1':'FLUX.3197', names_to='flux', names_prefix='FLUX.', values_to='luminosity') 
kepler_train$flux <- as.integer(as.character(kepler_train$flux))

kepler_train_small <- kepler_train %>%
  filter(category == 2 | star < (38 + 37))


kepler_train_small %>%
  filter(star == 1) %>%
  filter(luminosity > -1000) %>%
  ggplot(aes(x=flux, y=luminosity)) +
  geom_point() +
  scale_x_continuous(breaks=seq(0,3197,1000))
  
