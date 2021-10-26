library(tidyverse)
library(data.table)

lcc_latency <- as.data.frame(fread(file="/Users/willmee/tmp/lcc-pings.csv", header=TRUE))

ggplot(lcc_latency, aes(time_total)) +
  geom_histogram(binwidth = 0.5, color = "black", fill="orange") +
  xlab("Latencia (segundos)") +
  ylab("Cuenta") +
  ggtitle("Latencia de pagina principal de LCC")
  
ggplot(lcc_latency, aes(time_total)) +
  geom_histogram(binwidth = 0.5, color = "black", fill="orange") +
  xlab("Latencia (segundos)") +
  ylab("Cuenta") +
  ggtitle("Latencia de pagina principal de LCC")

quantile(lcc_latency$time_total, c(.95))

ggplot(lcc_latency, aes(time_total)) +
  stat_ecdf()