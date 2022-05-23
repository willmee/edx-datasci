# Q1
nrow(edx)
ncol(edx)
# 9000055 x 6

# Q2
sum(edx$rating == 0)
# 0
sum(edx$rating == 3)
# 2121240

# Q3
n_distinct(edx$movieId)
# 10677

# Q4
n_distinct(edx$userId)
# 69878

# Q5
count_genre <- function(genre) {
  edx %>%
  filter(grepl(genre, genres)) %>%
  nrow()
}

count_genre('Drama')
# 3910127
count_genre('Thriller')
# 2325899
count_genre('Romance')
# 1712100

# Q6
edx %>%
  group_by(title) %>%
  summarize(ratings = n()) %>%
  arrange(desc(ratings)) %>%
  head()

# Q7 
rating_counts <- edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
rating_counts

# Q8
sum(rating_counts$count[round(rating_counts$rating) == rating_counts$rating])
# 7156885
sum(rating_counts$count[round(rating_counts$rating) != rating_counts$rating])
# 1843170

# answer:
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
