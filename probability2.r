library(gtools)
library(tidyverse)

set.seed(1)

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
events <- replicate(B, {
  sum(sample(runners, 3) == "Jamaica") == 3
})
mean(events)

# 1.4 Question 2: Restaurant management
entrees <- paste("entree", as.character(1:6))
drinks <- paste("drinks", as.character(1:2))
sides <- paste("sides", as.character(1:6))
nrow(combinations(6,3)) * 6 * nrow(combinations(3, 1))

meal_combos <- function(num_entrees) {
  nrow(combinations(num_entrees, 1)) * nrow(combinations(6, 2)) * nrow(combinations(3, 1))
}

meal_combos(6)
num_entrees <- seq(1, 12)
data.frame(num_entrees=num_entrees, num_combos=sapply(num_entrees, meal_combos))

meal_combos2 <- function(num_sides) {
  nrow(combinations(6, 1)) * nrow(combinations(num_sides, 2)) * nrow(combinations(3, 1))
}
num_sides <- seq(2, 12)
data.frame(num_sides=num_sides, num_combos=sapply(num_sides, meal_combos2))


