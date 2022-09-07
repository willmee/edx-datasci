
# Requires registration at Kaggle - not a public dataset.
# https://www.kaggle.com/datasets/keplersmachines/kepler-labelled-time-series-data

# update as needed
setwd('/Users/willmee/dev/rlang/edx-datasci/capstone-idv')
data_dir <- paste(getwd(), 'data', sep='/')

# read in the data set
kepler_train <- read.csv(file = paste(data_dir, 'exoTrain.csv', sep='/'))
kepler_test <- read.csv(file = paste(data_dir, 'exoTest.csv', sep='/'))

kepler_train$LABEL <- as.factor(kepler_train$LABEL)
kepler_test$LABEL <- as.factor(kepler_test$LABEL)

# data exploration and visualization
# 5087 x 3198
dim(kelper_train)
# 570 x 3198
dim(kelper_test)
