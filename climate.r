library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(year)


temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(.$year)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  min()

minmax_emmissions <- temp_carbon %>%
  filter(!is.na(carbon_emissions) & (year == 1751 | year == 2014)) %>%
  pull(carbon_emissions) 
  
minmax_emmissions[1]/minmax_emmissions[2]
  
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  pull(year) %>%
  min()

temp_carbon %>%
  filter(!is.na(temp_anomaly) & (year == 1880 | year == 2018)) %>%
  pull(temp_anomaly)

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line(aes(colour="orange")) + 
  geom_hline(aes(yintercept = 0), col = "blue") + 
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") +
  theme(panel.grid.minor = element_line(colour="white", size=0.2)) +
  scale_x_continuous(breaks = seq(1880, 2020, 20), minor_breaks = seq(1880 , 2020, 5))

temp_carbon_gathered <- gather(temp_carbon, key = anomaly_type, value = anomaly, 
             c("temp_anomaly", "land_anomaly", "ocean_anomaly"))
head(temp_carbon_gathered)
head(temp_carbon)

temp_carbon_gathered %>%
  filter(!is.na(anomaly)) %>%
  ggplot(aes(year, anomaly, group=anomaly_type, color=anomaly_type)) +
  geom_line() + 
  geom_hline(aes(yintercept = 0), col = "blue") + 
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") +
  theme(panel.grid.minor = element_line(colour="white", size=0.2)) +
  scale_x_continuous(breaks = seq(1880, 2020, 20), minor_breaks = seq(1880 , 2020, 5))
