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
luminosity_timeline(c(seq(1, 2), seq(38, 39)), 2)

# Stars without exoplanets
luminosity_timeline(seq(38, 49))

# scale all luminosities to be a ratio of the max

kepler_train <- kepler_train %>%
  group_by(star) %>%
  mutate(luminosity_ratio=(luminosity - min(luminosity))/(max(luminosity)-min(luminosity))) %>%
  ungroup()

# TODO: just filter
kepler_train_small <- kepler_train_small %>%
  group_by(star) %>%
  mutate(luminosity_ratio=(luminosity - min(luminosity))/(max(luminosity)-min(luminosity))) %>%
  ungroup()


# Calculate aggregate data for each star
luminosity_agg_small = kepler_train_small %>%
  group_by(star, category) %>% 
  summarise(mean=mean(luminosity), sd=sd(luminosity), 
            mean_ratio=mean(luminosity_ratio), sd_ratio=sd(luminosity_ratio))
luminosity_agg = kepler_train %>%
  group_by(star, category) %>% 
  summarise(mean=mean(luminosity), sd=sd(luminosity), 
            mean_ratio=mean(luminosity_ratio), sd_ratio=sd(luminosity_ratio))

# visualize one or more luminosity distribution
luminosity_density <- function(star_range, ncol=4) {
  kepler_train_small %>%
    filter(star %in% star_range) %>%
    ggplot(aes(x=luminosity, fill=category, color=category)) +
    geom_density(alpha=0.4) +
    scale_fill_manual(values = category_colors) +
    scale_color_manual(values = category_colors) +
    facet_wrap(vars(star), ncol=ncol, scales='free', labeller='label_both') +
    geom_vline(data=luminosity_agg_small  %>% filter(star %in% star_range),
               aes(xintercept=mean, color=category),
               linetype="dotted") +
    geom_vline(data=luminosity_agg_small  %>% filter(star %in% star_range),
             aes(xintercept=mean + sd, color=category),
             linetype="dashed") +
    geom_vline(data=luminosity_agg_small  %>% filter(star %in% star_range),
             aes(xintercept=mean - sd, color=category),
             linetype="dashed") +
    theme(legend.position = "none")
}

luminosity_density(c(seq(1, 10), seq(38, 47)), 4)

# Are there differences in the standard deviations?
luminosity_agg %>% 
  ggplot(aes(x=category, y=sd_ratio, color=category)) +
  geom_point(alpha=0.7, size=3) +
  scale_color_manual(values = category_colors) +
  theme(legend.position = 'none') +
  ggtitle('std deviation by category') +
  ylab('scaled std deviation')

# Analysing seasonality
# Start with decomposition and visualization
luminosity_ts <- ts(kepler_train_small %>%
  filter(star == 1) %>%
  select(luminosity) %>%
  pull(), start=1, end=3197, frequency=3197)
luminosity_decomp <- decompose(luminosity_ts, 'additive')
