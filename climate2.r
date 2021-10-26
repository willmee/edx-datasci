library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(xintercept = 1850, color="red") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

# carbon emissions
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  geom_vline(xintercept = 1850, color="red") +
  theme(panel.grid.minor = element_line(colour="white", size=0.2)) +
  scale_x_continuous(breaks = seq(1800, 2020, 20), minor_breaks = seq(1800 , 2020, 5)) +
  ylab("Metric tonnes") 
  
co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, group=source, color=source)) +
  geom_line()

co2_time +
  xlim(-800000, -775000)
  
co2_time +
  xlim(-375000, -330000)

co2_time +
  xlim(-3000, 2018)
