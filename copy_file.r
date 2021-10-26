library(tidyverse)    # includes readr
library(readxl)

# see working directory
getwd()

# change your working directory
#setwd()

# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package="dslabs")
list.files(path)

# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

# copy file from dslabs package to your working directory
file.copy(fullpath, paste(getwd(), 'data', sep='/'))

dir_filename <- paste('data', filename, sep='/')

# check if the file exists
file.exists(dir_filename)

# inspect the first 3 lines
read_lines(dir_filename, n_max = 3)

# read file in CSV format
dat <- read_csv(dir_filename)

head(dat)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
breast_cancer <- read_csv(url, col_names=FALSE)
