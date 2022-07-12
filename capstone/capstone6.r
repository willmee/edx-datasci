# Explore the RecommenderLab package
# https://www.rdocumentation.org/packages/recommenderlab/versions/1.0.1
# https://cran.r-project.org/web/packages/recommenderlab/recommenderlab.pdf
# 
# See also e.g.
# https://michael.hahsler.net/other_courses/ICMA_Recommendation_Tools/code/UBCF.html

# install.packages("recommenderlab")

library("recommenderlab")
library(tidyverse)

data("MovieLense")

#load(file="/Users/willmee/dev/rlang/edx-datasci/data/movielens-capstone.Rda")
#load(file="/Users/willmee/dev/rlang/edx-datasci/data/movielens-capstone-validation.Rda")

MovieLense100 <- MovieLense[rowCounts(MovieLense) > 100, ]
MovieLense100
train <- MovieLense100[1:100]
recommenderRegistry$get_entry("UBCF", dataType="realRatingMatrix")
rec <- Recommender(train, method = "UBCF")
#pre <- predict(rec, MovieLense100[101:102], n = 5)
pre <- predict(rec, MovieLense100[101:102], type="ratings")
pre <- predict(rec, MovieLense100[101:102], type="ratingMatrix")
pre
as(pre, "matrix")[,1:10]
getRatings(pre)
getRatingMatrix(pre)[,1:10]
