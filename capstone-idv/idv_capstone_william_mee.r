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

# Data exploration
# 4176 entries
dim(abalone_df)

# comparable numbers of female, male and infantile observations
by_sex_summary <- abalone_df %>%
  group_by(Sex) %>%
  summarize(
    Count=n(), RingsMean=mean(Rings), RingsSd=sd(Rings),
    RingsWholeWeightCor=cor(Rings, WholeWeight),
    RingsShuckedWeightCor=cor(Rings, ShuckedWeight),
    RingsVisceraWeightCor=cor(Rings, VisceraWeight),
    RingsShellWeightCor=cor(Rings, ShellWeight)
  )
# from the correlation, it looks like the WholeWeight is a good feature. Also 
# interesting that immature is more strongly correlated in all the features: these
# abalone are presumably growing.

# ring histograms grouped by sex
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
  scale_colour_brewer(palette = 'Set2') +
  labs(title='Abalone Ring Count vs Whole Weight Variance', x='Rings', y = 'Whole Weight') 
  