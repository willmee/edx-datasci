# Use of Recommenderlab with the full data
# https://www.rdocumentation.org/packages/recommenderlab/versions/1.0.1
# https://cran.r-project.org/web/packages/recommenderlab/recommenderlab.pdf
# https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf
# 
# See also e.g.
# https://michael.hahsler.net/other_courses/ICMA_Recommendation_Tools/code/UBCF.html

library("recommenderlab")
library(tidyverse)

data_dir <- "/Users/WillMee/dev/personal/edx-datasci/capstone/data"
load(file=paste(data_dir, "movielens-capstone.Rda", sep="/"))
load(file=paste(data_dir, "movielens-capstone-validation.Rda", sep="/"))

# ratio of NA to entries with values 
matrix_sparcity <- function(matrix) {
  matrix_dim <- dim(matrix)  
  sum(is.na(matrix))/(matrix_dim[1] * matrix_dim[2])
}

# return the ratio is NA entries in predicted which are not NA in actual. 
na_ratio <- function(actual, predicted) {
  stopifnot(dim(actual) == dim(predicted))
  predicted_cp <- predicted
  predicted_cp[is.na(actual)] <- -1
  matrix_sparcity(predicted_cp)
}

# RMSE supporting NA values
rmse_matrix <- function(actual, predicted) {
  stopifnot(dim(actual) == dim(predicted))
  actual_cp <- actual
  predicted_cp <- predicted
  nas <- is.na(actual)
  predicted_cp[nas] <- -1
  residual_na_ratio <- matrix_sparcity(predicted_cp)
  actual_cp[nas] <- -1
  residual_nas <- is.na(predicted_cp)
  predicted_cp[residual_nas] <- -2
  actual_cp[residual_nas] <- -2
  list(rmse = sqrt(sum((actual_cp - predicted_cp)^2)/(sum(!(nas | residual_nas)))), na_ratio=residual_na_ratio)
}

rmse_original <- function(actual, predicted) {
  stopifnot(dim(actual) == dim(predicted))
  actual_cp <- actual
  predicted_cp <- predicted
  nas <- is.na(actual)
  predicted_cp[nas] <- -1
  residual_na_ratio <- matrix_sparcity(predicted_cp)
  actual_cp[nas] <- -1
  list(rmse = sqrt(sum((actual_cp - predicted_cp)^2)/(sum(!nas))), na_ratio=residual_na_ratio)
}

test_func <- function(actual, predicted) {
  dim(predicted)
  dim(actual) == dim(predicted)
}  

# convert the data into a 69878x10678 users x movies matrix
train_matrix <- edx %>% 
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

# add row names
rownames(train_matrix) <- train_matrix[,1]
train_matrix <- train_matrix[,-1]
# 69878 x 10677 users x movies 
dim(train_matrix)

test_matrix <- validation %>% 
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

rownames(test_matrix) <- test_matrix[,1]
test_matrix <- test_matrix[,-1]
dim(test_matrix)
# 68534 x 9809 users x movies

movie_id_delta <- setdiff(unique(edx$movieId), unique(validation$movieId))

test_matrix_ext <- matrix(data=NA, nrow=dim(test_matrix)[1], ncol=length(movie_id_delta))
colnames(test_matrix_ext) <- movie_id_delta
rownames(test_matrix_ext) <- rownames(test_matrix)
test_matrix <- cbind(test_matrix, test_matrix_ext)
# ensure column order is the same in both matrices
test_matrix <- test_matrix[,colnames(train_matrix)]

# 68534 x 10677
dim(test_matrix)

# clean up
rm(test_matrix_ext)
rm(edx)
rm(validation)
rm(movie_id_delta)

# setup recommender lab data structures
train_rrm <- as(train_matrix, "realRatingMatrix")
# rm(train_matrix)
test_rrm <- as(test_matrix, "realRatingMatrix")
#test_rrm_part <- as(test_matrix[1:100,], "realRatingMatrix")
test_rrm_part <- as(test_matrix[1:1000,], "realRatingMatrix")
test_rrm_part_normalized <- normalize(test_rrm_part)


# use RecommenderLab
rec_ubcf <- Recommender(train_rrm, method = "UBCF")
rec_random <- Recommender(train_rrm, method = "RANDOM")
rec_svd <- Recommender(train_rrm, method="SVD")
system.time(rec_svdf <- Recommender(train_rrm, method="SVDF"))
# slow - 12-36 hours?
rec_ibcf <- Recommender(train_rrm, method="IBCF")
rec_popular <- Recommender(train_rrm, method="POPULAR")
rec_ar <- Recommender(train_rrm, method="AR")

# rm(train_rrm)
system.time(pre <- predict(rec, test_rrm, type="ratingMatrix"))
system.time(pre_random_part <- predict(rec_random, test_rrm_part, type="ratingMatrix"))
# UBCF part timing:
# user   system  elapsed 
# 1980.621   25.609 1850.962 
# i.e. about 30 minutes for 1000 rows.
system.time(pre_ubcf_part <- predict(rec_ubcf, test_rrm_part, type="ratingMatrix"))
# SVD part timing
# user  system elapsed 
# 0.487   0.105   0.592
system.time(pre_svd_part <- predict(rec_svd, test_rrm_part, type="ratingMatrix"))

# SVD on complete set
# user  system elapsed 
# 59.829  45.374 143.881 
system.time(pre_svd <- predict(rec_svd, test_rrm, type="ratingMatrix"))

#user  system elapsed 
#0.144   0.082   0.226 
system.time(pre_ibcf_part <- predict(rec_ibcf, test_rrm_part, type="ratingMatrix"))
#user  system elapsed 
#15.141  17.438  42.188 
system.time(pre_ibcf <- predict(rec_ibcf, test_rrm, type="ratingMatrix"))

pre_ibcf_part_m <- as(pre_ibcf_part, "matrix")
pre_ibcf_part_m[1:10,1:20]

# 0.6703336/0.0007229559
rmse_matrix(test_matrix[1:1000,],as(pre_ubcf_part, "matrix"))
# 0.8318634/0
rmse_matrix(test_matrix[1:1000,],as(pre_svd_part, "matrix"))
# Entire SVD result:
# 0.8356373/0
rmse_matrix(test_matrix,as(pre_svd, "matrix"))

# 2/0.001268989
rmse_matrix(test_matrix[1:1000,],as(pre_ibcf_part, "matrix"))
# 0.9867809/0.001362822
rmse_matrix(test_matrix,as(pre_ibcf, "matrix"))

# Calculate full UBCF iteratively
#save(pre_ubcf_part, file = "/Users/WillMee/dev/personal/edx-datasci/capstone/data/ubcf-1.Rda")

chunk_size <- 1000
num_rows <- dim(test_matrix)[1]
start_rows <- seq(from=4001, to=num_rows, by=chunk_size)
start_ts <- Sys.time()
for(start_row in start_rows) { 
  chunk_start_ts <- Sys.time()
  end_row <- min(start_row+chunk_size-1, num_rows)
  print(paste("[", format(Sys.time(), "%x %X"), "] ", start_row, "-", end_row, "/", num_rows, sep=""))
  test_rrm_part <- as(test_matrix[start_row:end_row,], "realRatingMatrix")
  system.time(pre_ubcf_part <- predict(rec_ubcf, test_rrm_part, type="ratingMatrix"))
  chunk_file <- paste(data_dir, "/ubcf-", start_row, ".Rda", sep="")
  save(pre_ubcf_part, file = chunk_file)
  print(paste("wrote ", chunk_file, sep=""))
  print(Sys.time() - chunk_start_ts)
}
end_ts <- Sys.time()
print(paste("total time ", (end_ts - start_ts), sep=""))

pre_ubcf_matrix <- matrix(, nrow=0, ncol=dim(test_matrix)[2], byrow=TRUE)
# merge saved UBCF data
for(start_row in start_rows) { 
  chunk_file <- paste(data_dir, "/ubcf-", start_row, ".Rda", sep="")
  load(file = chunk_file)
  print(paste("read ", chunk_file))
  pre_ubcf_matrix <- rbind(pre_ubcf_matrix, as(pre_ubcf_part, "matrix"))
  print(dim(pre_ubcf_matrix))
}
rmse_matrix(test_matrix[1:num_rows,], pre_ubcf_matrix)
