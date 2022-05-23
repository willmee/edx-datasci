library(dslabs)
data(heights)
heights
class(heights)
str(heights)

heights %>%
  summary(mean = mean(height), median = median(height))

heights %>%
  summarise(mean = mean(height), median = median(height))


median(heights$height)