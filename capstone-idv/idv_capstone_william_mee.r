library(caret)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

# download the Abalone dataset
# See https://archive.ics.uci.edu/ml/datasets/abalone for more information
abalone_tmp <- tempfile()
download.file('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', abalone_tmp)
# add column names etc.
abalone_df <- read.csv(file = abalone_tmp)
# Names are obtained from 
# https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.names
colnames(abalone_df) <- c(
  'Sex',
  'Length',
  'Diameter', 
  'Height',
  'WholeWeight', 
  'ShuckedWeight', 
  'VisceraWeight',
  'ShellWeight', 
  'Rings'
  )
abalone_df$Sex <- as.factor(abalone_df$Sex)

color_palette <- 'Set2'

# Data exploration
# 4176 entries
dim(abalone_df)

# rings range from 1 to 29
max(abalone_df$Rings)
min(abalone_df$Rings)

# comparable numbers of female, male and infantile observations
by_sex_summary <- abalone_df %>%
  group_by(Sex) %>%
  summarize(
    Count=n(), 
    RingsMean=mean(Rings), 
    RingsSd=sd(Rings)
  )

# Correlations
abalone_df %>%
  group_by(Sex) %>%
  summarize(
    RingsWholeWeightCor=cor(Rings, WholeWeight),
    RingsShuckedWeightCor=cor(Rings, ShuckedWeight),
    RingsVisceraWeightCor=cor(Rings, VisceraWeight),
    RingsShellWeightCor=cor(Rings, ShellWeight),
    RingsLengthCor=cor(Rings, Length),
    RingsHeightCor=cor(Rings, Height),
    RingsDiameterCor=cor(Rings, Diameter)
  )
# from the correlation, it looks like WholeWeight and Height are good features. Also 
# interesting that immature set is more strongly correlated in all the features: these
# abalone are growing.

# ring histograms grouped by sex.
# The better correlation with the infantile abalone is clear
abalone_df %>%
  ggplot(aes(x=Rings, color=Sex)) +
  geom_histogram(binwidth=1, aes(fill=Sex), alpha=0.5) +
  geom_vline(data=by_sex_summary, aes(xintercept=RingsMean),
             linetype='dashed',  color='Red') +
  labs(title='Abalone Rings Histograms by Sex with Means', x='Rings', y = 'Count') +
  scale_colour_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  facet_grid(Sex ~ .)

# scatter plots of rings vs whole weight
abalone_df %>%
  ggplot(aes(x=Rings, y=WholeWeight, color=Sex)) +
  geom_point(aes(fill=Sex), alpha=0.8) +
  scale_colour_brewer(palette = color_palette) +
  labs(title='Abalone Ring Count vs Whole Weight', x='Rings', y = 'Whole Weight (g)') 

# scatter plot of rings vs height
# shows some outliers
abalone_df %>%
  ggplot(aes(x=Rings, y=Height, color=Sex)) +
  geom_point(aes(fill=Sex), alpha=0.8) +
  scale_colour_brewer(palette = color_palette) +
  labs(title='Abalone Ring Count vs Height', x='Rings', y = 'Height (mm)') 

# Split into train and test sets
set.seed(51, sample.kind = 'Rounding')
test_index <- createDataPartition(abalone_df$Rings, times = 1, p = 0.1, list = FALSE)
# 419 samples
abalone_test <- abalone_df[test_index, ]
# 3757 samples
abalone_train <- abalone_df[-test_index, ]  

# error functions
abalone_rmse <- function(y_hat) {
  sqrt(sum((round(y_hat, digits=0) - abalone_test$Rings)^2)/length(y_hat))
}

abalone_rmse_no_round <- function(y_hat) {
  sqrt(sum((y_hat - abalone_test$Rings)^2)/length(y_hat))
}


# Train a first model using the caret package
train_glm <- train(Rings ~ ., method = 'glm', data = abalone_train)
y_hat_glm <- predict(train_glm, abalone_test, type = 'raw')

# 2.147319
abalone_rmse(y_hat_glm)

# look at the confusion matrix which is broken down per ring
cf_glm <- confusionMatrix(
  data = factor(round(y_hat_glm), seq(1, 29)), 
  reference = factor(abalone_test$Rings, seq(1, 29))
)

