library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[1]])

tab_1 = html_table(nodes[[10]])
tab_2 = html_table(nodes[[19]])
tab_1_clean <- tab_1 %>% filter(X2 != "Team") %>% select(-X1) %>% rename(Team = X2, Payroll = X3, Average = X4)
tab_2_clean <- tab_2 %>% filter(X1 != 'Team') %>% rename(Team = X1, Payroll = X2, Average = X3)

library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
brexit_html <- read_html(url)
brexit_nodes <- html_nodes(brexit_html, "table")
examine_table <- function(node) {
  tab <- html_table(node)
  #dim(tab)
  tab
}

sapply(brexit_nodes[1:10], examine_table)