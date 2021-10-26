library(tidyverse)
library(pdftools)
options(digits = 3)

# On September 20, 2017, Hurricane María made landfall on Puerto Rico. It was 
# the worst natural disaster on record in Puerto Rico and the deadliest Atlantic 
# hurricane since 2004. However, Puerto Rico's official death statistics only 
# tallied 64 deaths

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
#system2("open", args = fn)
txt <- pdf_text(fn)
x <-txt[9] %>% str_split('\n')
s <- x[[1]]
s <- str_trim(s)
str_which(s, '2015')
header <- s[3]
header <- header %>% str_split('\\s+', simplify=TRUE)
header <- header[2:5]

str_which(s, 'Total')
sum(str_count(s, '^\\d+$'))

# remove unwanted proceeding and trailing rows
last_wanted_idx <- str_which(s, 'Total')-1
s <- s[4:last_wanted_idx]
# remove single number rows
s <- s[- str_which(s, '^\\d+$')]
s <- str_remove_all(s, "[^\\d\\s]")
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

# convert into a data frame
tab <- s %>% as_tibble() %>% setNames(c('day', header))
tab[] <- sapply(tab, as.numeric)
tab <- cbind(month = month, tab)
mean(tab$`2015`)
# tidy form, without month
tab %>%  subset(select = -c(month)) %>%gather(year, deaths, -day)