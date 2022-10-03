library(caret)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

# Download the Abalone dataset
# See https://archive.ics.uci.edu/ml/datasets/abalone for more information
abalone_tmp <- tempfile()
download.file('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', abalone_tmp)
# Convert to a dataframe and rename columns
abalone_df <- read.csv(file = abalone_tmp)
colnames(abalone_df) <- c(
  'Sex', 'Length', 'Diameter', 'Height', 'WholeWeight', 
  'ShuckedWeight', 'VisceraWeight', 'ShellWeight', 'Rings'
  )
# Convert the Sex column to a factor
abalone_df$Sex <- as.factor(abalone_df$Sex)
# Save a local copy 
save(abalone_df, file='./edx-datasci/capstone-idv/data/abalone.Rda')

# Load the local copy
load('./edx-datasci/capstone-idv/data/abalone.Rda')

# Remove two outlier samples
abalone_df <- abalone_df %>% filter(Height <= 0.5)



# Data exploration
# 4174 entries
dim(abalone_df)

# Summary by Sex
by_sex_summary <- abalone_df %>%
  group_by(Sex) %>%
  summarize(
    Count=n(), 
    RingsMin=min(Rings), 
    RingsMean=mean(Rings), 
    RingsMax=max(Rings), 
    RingsSd=sd(Rings),
    ShellWeightMean=mean(ShellWeight), 
  )

# rings range from 1 to 29
abalone_df %>%
  summarize(
    minRings = min(Rings), maxRings = max(Rings),
    minWholeWeight = min(WholeWeight), maxWholeWeight = max(WholeWeight),
  )

# Correlations
curr_digits = getOption('digits', default = 5)
options(digits = 3)
abalone_df %>%
  summarize(
    RingsRingsCor=cor(Rings, Rings),
    RingsLengthCor=cor(Rings, Length),
    RingsDiameterCor=cor(Rings, Diameter),
    RingsHeightCor=cor(Rings, Height),
    RingsWholeWeightCor=cor(Rings, WholeWeight),
    RingsShuckedWeightCor=cor(Rings, ShuckedWeight),
    RingsVisceraWeightCor=cor(Rings, VisceraWeight),
    RingsShellWeightCor=cor(Rings, ShellWeight),
  )

abalone_df %>%
  group_by(Sex) %>%
  summarize(
    RingsHeightCor=cor(Rings, Height),
  )

options(digits = curr_digits)

# from the correlation, it looks like ShellWeight, Diameter and Height are good features. Also 
# interesting that immature set is more strongly correlated in all the features: these
# abalone are growing.

color_palette <- 'Set2'

# ring histograms grouped by sex.
# The better correlation with the infantile abalone is clear
abalone_df %>%
  ggplot(aes(x=Rings, color=Sex)) +
  geom_histogram(binwidth=1, aes(fill=Sex), alpha=0.5) +
  geom_vline(data=by_sex_summary, aes(xintercept=RingsMean),
             linetype='dashed',  color='Red') +
  labs(title='Abalone Rings Histograms by Sex with Means', x='Rings', y = 'Count') +
  scale_colour_brewer(palette = color_palette) +
  scale_fill_brewer(palette = color_palette) +
  facet_grid(Sex ~ .)

# scatter plots of rings vs shell weight
abalone_df %>%
  ggplot(aes(x=Rings, y=ShellWeight, color=Sex)) +
  geom_point(aes(fill=Sex), alpha=0.8) +
  scale_colour_brewer(palette = color_palette) +
  labs(title='Abalone Ring Count vs Shell Weight', x='Rings', y = 'Whole Weight (kg)') 

# scatter plot of rings vs height
# shows some outliers
abalone_df %>%
  ggplot(aes(x=Rings, y=Height, color=Sex)) +
  geom_point(aes(fill=Sex), alpha=0.8) +
  scale_colour_brewer(palette = color_palette) +
  labs(title='Abalone Ring Count vs Height', x='Rings', y = 'Height (m)') 

# Histograms of shell weight by sex
abalone_df %>%
  ggplot(aes(x=ShellWeight, color=Sex)) +
  geom_histogram(aes(fill=Sex), alpha=0.5) +
  geom_vline(data=by_sex_summary, aes(xintercept=ShellWeightMean),
             linetype='dashed',  color='Red') +
  labs(title='Abalone ShellWeight Histograms by Sex with Means', x='ShellWeight', y = 'Count') +
  scale_colour_brewer(palette = color_palette) +
  scale_fill_brewer(palette = color_palette) +
  facet_grid(Sex ~ .)



# Boxplots to look at outliers
abalone_df %>%
  ggplot(aes(x=Rings, y=WholeWeight, group=Rings)) + 
  geom_boxplot(outlier.colour="red")

abalone_df %>%
  ggplot(aes(x=Rings, y=Height, group=Rings)) + 
  geom_boxplot(outlier.colour="red")

# two outliers
abalone_df %>% filter(Height>0.5)

abalone_df %>%
  ggplot(aes(x=Rings, y=Diameter, group=Rings)) + 
  geom_boxplot(outlier.colour="red")


# Remove outliers and split into train and test sets
set.seed(51, sample.kind = 'Rounding')
test_index <- createDataPartition(abalone_df$Rings, times = 1, p = 0.1, list = FALSE)
# 418 samples
abalone_test <- abalone_df[test_index, ]
# 3756 samples
abalone_train <- abalone_df[-test_index, ]  

# error functions
abalone_rmse <- function(y_hat) {
  sqrt(sum((round(y_hat, digits=0) - abalone_test$Rings)^2)/length(y_hat))
}

abalone_rmse_no_round <- function(y_hat, y) {
  sqrt(sum((y_hat - abalone_test$Rings)^2)/length(y_hat))
}

# Train a first model using the caret package
train_glm <- train(Rings ~ ., method = 'glm', data = abalone_train)
y_hat_glm <- predict(train_glm, abalone_test, type = 'raw')

# 2.147319
abalone_rmse(y_hat_glm)

# accuracy: 0.221957
mean(round(y_hat_glm) == abalone_test$Rings)

# look at the confusion matrix which is broken down per ring
cm_glm <- confusionMatrix(
  data = factor(round(y_hat_glm), seq(1, 29)), 
  reference = factor(abalone_test$Rings, seq(1, 29))
)

cm_glm$overall
cm_glm$byClass[,c("Sensitivity","Specificity", "Prevalence")]

# Are the immature abalone easier to classify?
abalone_train_i <- abalone_train %>% filter(Sex == 'I') %>% select(!Sex)
abalone_test_i <- abalone_test %>% filter(Sex == 'I') %>% select(!Sex)
abalone_train_m <- abalone_train %>% filter(Sex == 'M') %>% select(!Sex)
abalone_test_m <- abalone_test %>% filter(Sex == 'M') %>% select(!Sex)
abalone_train_f <- abalone_train %>% filter(Sex == 'F') %>% select(!Sex)
abalone_test_f <- abalone_test %>% filter(Sex == 'F') %>% select(!Sex)


train_glm_i <- train(Rings ~ ., method = 'glm', data = abalone_train_i)
y_hat_glm_i <- predict(train_glm_i, abalone_test_i, type = 'raw')

# yes: accuracy = 0.3154362 
mean(round(y_hat_glm_i) == abalone_test_i$Rings)

# Looking at kNN
train_knn <- train(Rings ~ ., method = 'knn', data = abalone_train)
y_hat_knn <- predict(train_knn, abalone_test, type = 'raw')
# 2.190237
abalone_rmse(y_hat_knn)

cm_knn <- confusionMatrix(
  data = factor(round(y_hat_knn), seq(1, 29)), 
  reference = factor(abalone_test$Rings, seq(1, 29))
)
# 0.2649165
cm_knn$overall['Accuracy']

train_knn_i <- train(Rings ~ ., method = 'knn', data = abalone_train_i, tuneGrid = data.frame(k = 27))
rings_pred_knn_i <- predict(train_knn_i, abalone_test_i, type = 'raw')
 0.3825503
mean(round(y_hat_knn_i) == abalone_test_i$Rings)

train_knn_m <- train(Rings ~ ., method = 'knn', data = abalone_train_m, tuneGrid = data.frame(k = 27))
rings_pred_knn_m <- predict(train_knn_m, abalone_test_m, type = 'raw')
# 0.2108844
mean(round(y_hat_knn_m) == abalone_test_m$Rings)

train_knn_f <- train(Rings ~ ., method = 'knn', data = abalone_train_f, tuneGrid = data.frame(k = 27))
rings_pred_knn_f <- predict(train_knn_f, abalone_test_f, type = 'raw')
# only 0.1869919
mean(round(y_hat_knn_f) == abalone_test_f$Rings)

rings_pred_knn_recombined = c(rings_pred_knn_i, rings_pred_knn_m, rings_pred_knn_f)
abalone_rings_test_recombined = c(abalone_test_i$Rings, abalone_test_m$Rings, abalone_test_f$Rings)
sqrt(sum((round(rings_pred_knn_recombined, digits=0) - abalone_rings_test_recombined)^2)/length(rings_pred_knn_recombined))

# k parameter
train_knn <- train(Rings ~ ., method = 'knn', 
                   data = abalone_train,
                   tuneGrid = data.frame(k = seq(3, 71, 2)))
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune

#train_knn <- train(Rings ~ ., method = 'knn', data = abalone_train)
y_hat_knn <- predict(train_knn, abalone_test, type = 'raw')
# 2.166682
# now: 2.175879
abalone_rmse(y_hat_knn)
# 0.24821
# now: 0.2942584
mean(round(y_hat_knn) == abalone_test$Rings)


# Try using fewer features
train_knn_minimal <- train(Rings ~ ShellWeight + Diameter + Height, method = 'knn', 
                   data = abalone_train,
                   tuneGrid = data.frame(k = seq(15, 30, 2)))
y_hat_knn_minimal <- predict(train_knn_minimal, abalone_test, type = 'raw')
# 2.23 - error is significantly worse
abalone_rmse(y_hat_knn_minimal)

# Look at some alternative algorithms
# QDA
train_qda <- train(Rings ~ ShellWeight + Diameter + Height, method = 'qda', data = abalone_train)

# Random forest
library(randomForest)
fit_rf <- randomForest(Rings ~ ., data = abalone_train) 
rings_hat_rf <- predict(fit_rf, abalone_test)

# 2.16
abalone_rmse(rings_hat_rf)
# 0.234
mean(round(rings_hat_rf) == abalone_test$Rings)

# delegating knn
train_rf_i <- randomForest(Rings ~ ., data = abalone_train_i)
rings_pred_rf_i <- predict(train_rf_i, abalone_test_i)

train_rf_m <- randomForest(Rings ~ ., data = abalone_train_m)
rings_pred_rf_m <- predict(train_rf_m, abalone_test_m)

train_rf_f <- randomForest(Rings ~ ., data = abalone_train_f)
rings_pred_rf_f <- predict(train_rf_f, abalone_test_f)


rings_pred_rf_recombined = c(rings_pred_rf_i, rings_pred_rf_m, rings_pred_rf_f)
abalone_rings_test_recombined = c(abalone_test_i$Rings, abalone_test_m$Rings, abalone_test_f$Rings)
# 2.205908
sqrt(sum((round(rings_pred_rf_recombined, digits=0) - abalone_rings_test_recombined)^2)/length(rings_pred_rf_recombined))

