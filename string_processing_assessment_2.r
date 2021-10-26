library(rvest)
library(tidyverse)
library(stringr)

schedule <- data.frame("day" = c("Monday", "Tuesday"), "staff" = c("Mandy, Chris and Laura","Steve, Ruth and Frank"))
tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ", simplify = TRUE)) %>% 
  unnest(cols=c("staff"))

url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE)

has_percent <- function(s) {
  str_match(s, "%")
}

clean_polls <- polls %>% filter(str_detect(Remain, "%")) %>%
  setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))
dim(clean_polls)