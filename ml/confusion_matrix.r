library(tidyverse)
library(caret)
library(dslabs)
data(heights)

male_heights <- heights %>%
  filter(sex == 'Male')
female_heights <- heights %>%
  filter(sex == 'Female')

n_female <- nrow(female_heights)

heights_50 <- 
  male_heights[sample.int(nrow(male_heights), n_female),] %>%
  rbind(female_heights)
heights_50 <- heights_50[sample(nrow(heights_50)),]

heights_77 <- heights[sample(nrow(heights), n_female * 2),]

# verify different prevalence
mean(heights_77$sex == 'Male')
mean(heights_50$sex == 'Male')

test_index_50 <- createDataPartition(heights_50$sex, times = 1, p = 0.5, list = FALSE)
test_set_50 <- heights_50[test_index_50, ]
train_set_50 <- heights_50[-test_index_50, ]

test_index_77 <- createDataPartition(heights_77$sex, times = 1, p = 0.5, list = FALSE)
test_set_77 <- heights_77[test_index_77, ]
train_set_77 <- heights_77[-test_index_77, ]

# random guess
y_hat_50 <- sample(c("Male", "Female"), length(test_index_50), replace = TRUE) %>% 
  factor(levels = levels(test_set_50$sex))
y_hat_77 <- sample(c("Male", "Female"), length(test_index_77), replace = TRUE) %>% 
  factor(levels = levels(test_set_77$sex))

# show that the accuracy, sensitivity (recall) and specificity aren't altered much, but the 
# pos predictive value (specificity) is.
confusionMatrix(data = y_hat_50, reference = test_set_50$sex)
confusionMatrix(data = y_hat_77, reference = test_set_77$sex)
