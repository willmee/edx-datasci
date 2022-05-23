library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, R_per_game = R/G, HR_per_game = HR/G)

# Question 1a
# Use runs (R) per game to predict average attendance.
# For every 1 run scored per game, average attendance increases by how much?
tidy(lm(avg_attendance ~ R_per_game, Teams_small))
tidy(lm(avg_attendance ~ HR_per_game, Teams_small))

# Question 1b
# Use number of wins to predict average attendance; do not normalize for number of games.
tidy(lm(avg_attendance ~ W, Teams_small))

# Question 1c
# Use year to predict average attendance.
# How much does average attendance increase each year?
tidy(lm(avg_attendance ~ yearID, Teams_small))

# Question 2
# What is the correlation coefficient for runs per game and wins?
Teams_small %>% summarize(r=cor(R_per_game, W))
# What is the correlation coefficient for home runs per game and wins?
Teams_small %>% summarize(r=cor(HR_per_game, W))

Teams_strata <- Teams_small %>%
  mutate(strata = round(W/10)) %>%
  filter(strata %in% 5:10)

# Question 3a
# How many observations are in the 8 win strata?
Teams_strata %>%
  filter(strata == 8) %>%
  summarize(n = n())

# Question 3b
# Calculate the slope of the regression line predicting average attendance given runs per game for each of the win strata.
# Which win stratum has the largest regression line slope?
Teams_strata %>%
  group_by(strata) %>%
  do(tidy(lm(avg_attendance ~ R_per_game, data = .))) %>% 
  ungroup %>%
  filter(term == 'R_per_game') %>%
  summarize(max_strata = strata[which.max(estimate)])


# Calculate the slope of the regression line predicting average attendance given HR per game for each of the win strata.
# Which win stratum has the largest regression line slope?
Teams_strata %>%
  group_by(strata) %>%
  do(tidy(lm(avg_attendance ~ HR_per_game, data = .))) %>% 
  ungroup %>%
  filter(term == 'HR_per_game') %>%
  summarize(max_strata = strata[which.max(estimate)])

# Question 3c 
Teams_strata %>%
  group_by(strata) %>%
  summarize(correlation = cor(avg_attendance, R_per_game)) 

# Question 4

# Fit a multivariate regression determining the effects of runs per game, home runs per game, wins, and year on average attendance. Use the original Teams_small wins column, not the win strata from question 3.
# What is the estimate of the effect of runs per game on average attendance?
#  
Teams_small %>%
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data=.)

# Question 5

# Use the multivariate regression model from Question 4. Suppose a team averaged 5 runs per game, 1.2 home runs per game, and won 80 games in a season.
# What would this team's average attendance be in 2002?
coeffs <- tidy(Teams_small %>%
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data=.)) %>%
  pull(estimate)

sum(coeffs * c(1, 5, 1.2, 80, 2002))

# What would this team's average attendance be in 1960?
sum(coeffs * c(1, 5, 1.2, 80, 1960))

# Question 6
# Use your model from Question 4 to predict average attendance for teams in 2002 in the original Teams data frame.
# What is the correlation between the predicted attendance and actual attendance?
Teams_predict <- Teams %>% 
  filter(yearID == '2002') %>%
  mutate(
    avg_attendance = attendance/G,
    R_per_game = R/G,
    HR_per_game = HR/G) %>%
  mutate(predicated_avg_attendance = coeffs[1]+(coeffs[2]*R_per_game)+(coeffs[3]*HR_per_game)+(coeffs[4]*W)+(coeffs[5]*2002)) 


Teams_predict %>%
  #select(avg_attendance, predicted_avg_attendance)
  summarize(c = cor(avg_attendance, predicated_avg_attendance))
  