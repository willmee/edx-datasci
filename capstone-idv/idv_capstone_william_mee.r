# download the dataset
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
