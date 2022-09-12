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

category_colors <- c('2' = 'orange', '1' = 'blue')

# visualize one or more timelines
luminosity_timeline <- function(star_range, ncol=4) {
  kepler_train_small %>%
    filter(star %in% star_range) %>%
    ggplot(aes(x=flux, y=luminosity, color=category)) +
    #ggplot(aes(x=flux, y=luminosity)) +
    geom_point(alpha=0.5) +
    scale_color_manual(values = category_colors) +
    geom_smooth(formula = y ~ x, method = loess, method.args = list(span = 0.05, degree = 1), 
                color='red') +
    scale_x_continuous(breaks=seq(0,3197,1000)) +
    facet_wrap(vars(star), ncol=ncol, scales='free_y', labeller='label_both')
}

# Stars with exoplanets
luminosity_timeline(seq(1, 5), 1)

# Stars without exoplanets
luminosity_timeline(seq(38, 49))

# visualize one or more luminosity distribution
luminosity_density <- function(star_range, ncol=4) {
  kepler_train_small %>%
    filter(star %in% star_range) %>%
    ggplot(aes(x=luminosity, fill=category, color=category)) +
    geom_density(alpha=0.4) +
    #geom_vline(aes(xintercept=grp.mean, color=category),
    #           linetype="dashed") +    
    scale_fill_manual(values = category_colors) +
    scale_color_manual(values = category_colors) +
    facet_wrap(vars(star), ncol=ncol, scales='free_y', labeller='label_both')
}

luminosity_density(c(c(1, 2), c(38, 39)), 2)