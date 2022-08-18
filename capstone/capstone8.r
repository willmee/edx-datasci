# Recommenderlab visualizations
# See https://r-craft.org/r-news/movie-recommendation-with-recommenderlab/

library('recommenderlab')

dl <- tempfile()
download.file('https://files.grouplens.org/datasets/movielens/ml-latest-small.zip', dl)

ratings_small <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-latest-small/ratings.csv"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
movies_small <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-latest-small/movies.csv"))),
                       col.names = c("movieId", "title", "genres"))
movies_small <- as.data.frame(movies_small) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens_small <- left_join(ratings_small, movies_small, by = "movieId")


head(ratings_small)

ratings_small_matrix <- ratings_small %>% 
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

ratings_small_rrm <- as(ratings_small_matrix, "realRatingMatrix")

ratings_small_norm <- normalize(ratings_small_rrm)

eval_sets <- evaluationScheme(data = ratings_small_norm,
                              method = "cross-validation",
                              k = 10,
                              given = 5,
                              goodRating = 4)

models_to_evaluate <- list(
  `SVD` = list(name = "RANDOM", param=NULL),
#  `IBCF Cosine` = list(name = "IBCF", 
#                        param = list(method = "cosine")),
  `IBCF Pearson` = list(name = "IBCF", 
                        param = list(method = "pearson")),
#  `UBCF Cosine` = list(name = "UBCF",
#                        param = list(method = "cosine")),
  `UBCF Pearson` = list(name = "UBCF",
                        param = list(method = "pearson")),
  `Random` = list(name = "RANDOM", param=NULL)
)

n_recommendations <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate, 
                         n = n_recommendations)

plot(list_results, legend="topright")
