library(ggplot2)
library(tidyverse)

model_results <- data.frame(
  models=c("Baseline", "Movie Bias", "User/Movie Bias", "Movie Bias Reg", "User/Movie Bias Reg", "Movie/User/Genre Bias Reg", "SVD", "ICBF"),
  rmse=c(1.061202,0.9439087,0.8653488, 0.9438542, 0.8648177, 0.8647045, 0.8356373, 0.9867809)) %>%
  arrange(desc(rmse))

model_results$models <- factor(model_results$models, levels = model_results$models)

ggplot(model_results, aes(x = models, y = rmse)) +
  geom_segment(aes(x = models, xend = models, y = 0.6, yend = rmse),
               color = "blue", lwd = 1, linetype = "dotted") +
  geom_point(size = 6, pch = 21, bg = 4, col = 1) +
  ylim(0.6, 1.1) +
#  scale_x_discrete(labels = model_results$models) +
  coord_flip() +
  theme_minimal()

