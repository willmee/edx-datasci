library(tidyverse)
library(dslabs)
library(RColorBrewer)
data(stars)
options(digits = 3)   # report 3 significant digits

stars %>%
  summarise(mean_mag=mean(magnitude), stddev_mag=sd(magnitude))

stars %>%
  ggplot(aes(magnitude)) +
  geom_density()

stars %>%
  ggplot(aes(temp)) +
  geom_histogram(binwidth = 1000, fill = "blue", col = "black") 

starslog10 <- stars %>%
  mutate(templog10=log10(temp), is_sun=star=="Sun")

starslog10 %>%
  ggplot(aes(templog10, magnitude)) +
  geom_point(aes(alpha=0.9)) +
  geom_text(nudge_x = 0.02, nudge_y=0.0, size=3, 
            aes(label=star, hjust="left", color=is_sun)) +
  geom_vline(xintercept = log(5000, 10), color="red") +
  scale_y_reverse() +  
  scale_x_reverse() +
  xlab("temperature (log10, reversed)") +
  ylab("magnitude (reversed)") +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("blue", "orange"))

starslog10 %>%
  ggplot(aes(templog10, magnitude)) +
  geom_point(aes(color=type)) +
  scale_y_reverse() +  
  xlab("temperature (log10)") +
  ylab("magnitude (reversed)") +
  scale_colour_brewer(palette = "Paired")