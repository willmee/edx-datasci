library(dslabs)
library(lubridate)
options(digits = 3) 

data(brexit_polls)

head(brexit_polls)

sum(month(brexit_polls$startdate) == 4)

# returns 14, incorrect?
sum(round_date(brexit_polls$enddate, unit='week') == 
        round_date(ymd('2016-06-12'), unit='week'))

sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12")

enddates <- data.frame(enddate=sort(brexit_polls$enddate), 
                       rounded=round_date(sort(brexit_polls$enddate), unit='week'))

brexit_polls %>% mutate(endday = weekdays(enddate)) %>% count(endday, sort=TRUE)

data(movielens)
movielens %>% 
  mutate(datetime=as_datetime(timestamp), ts_year=year(datetime)) %>%
  count(ts_year, sort=TRUE)

movielens %>% 
  mutate(datetime=as_datetime(timestamp), ts_hour=hour(datetime)) %>%
  count(ts_hour, sort=TRUE)

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata %>%
  filter(str_detect(title, 'Pride and Prejudice'))

gutenberg_works(title == 'Pride and Prejudice')
pandp <- gutenberg_download(1342)
words <- pandp %>% unnest_tokens(word, text)
words <- words %>%
  filter(!word %in% stop_words$word ) 
words <- words %>%
       filter(!str_detect(word, "\\d"))
nrow(words)


# solution code from EdX question 8; expected answer is
# 122342 or 122204, actual answer is 122359
# as a follow-on, questions 9 through 12 are broken
book <- gutenberg_download(1342)
words <- book %>%
  unnest_tokens(word, text)
nrow(words)

nrow(words %>% count(word, sort=TRUE) %>% filter(n >= 100))

afinn <- get_sentiments("afinn")

afinn_words = words %>% inner_join(afinn, by='word')
dim(afinn_words)
mean(afinn_words$value > 0)
count(afinn %>% filter(value == 4))
