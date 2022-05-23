library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Q1

# The type column of dat indicates whether students took classes in person
# ("inclass") or online ("online"). What proportion of the inclass group is 
# female? What proportion of the online group is female?
num_online <- sum(dat$type == 'online')

# !!! demonstrates showing percentage across the groups
# note the group_by add = TRUE, and that type_size is a vector/list
# so it has to be indexed (but all values are the same, so any index
# will work)
dat %>%
  #filter(type == 'online') %>%
  group_by(type) %>%
  mutate(type_size = n()) %>%
  group_by(sex, add = TRUE) %>%
  summarize(count = n(), pct=n()/type_size[1], type_size_len = length(type_size))

# Q2

# In the course videos, height cutoffs were used to predict sex. Instead of 
# height, use the type variable to predict sex. Assume that for each class type 
# the students are either all male or all female, based on the most prevalent 
# sex in each class type you calculated in Q1. Report the accuracy of your 
# prediction of sex based on type. You do not need to split the data into 
# training and test sets.

y_hat <- ifelse(dat$type == 'online', "Male", "Female")  %>% 
  factor(levels = levels(heights$sex))
mean(y_hat == dat$sex)

# Q3
table(y_hat, dat$sex)

# Q4
y <- dat$sex %>%
  factor(levels = levels(heights$sex))
y_hat <- ifelse(dat$type == 'online', "Male", "Female")  %>% 
  factor(levels = levels(heights$sex))

sensitivity(data = y_hat, reference = y)

# Q5
specificity(data = y_hat, reference = y)

# Q6
# What is the prevalence (% of females) in the dat dataset defined above?
mean(dat$sex == 'Female')


