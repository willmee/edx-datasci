library(dslabs)
library(ggplot2)
data("research_funding_rates")
research_funding_rates

# Question 1

# Construct a two-by-two table of gender (men/women) by award status (awarded/not) using the total numbers across all disciplines.
# What is the number of men not awarded?

applications_men <- sum(research_funding_rates$applications_men)
applications_women <- sum(research_funding_rates$applications_women)
awarded_men <- sum(research_funding_rates$awards_men)
awarded_women <- sum(research_funding_rates$awards_women)

funding_rates_2by2 <- data.frame(
  gender = c('men', 'women'), 
  awarded=c(awarded_men, awarded_women), 
  not_awarded = c(applications_men - awarded_men, applications_women - awarded_women)
)
funding_rates_2by2

# Question 2

# Use the two-by-two table from Question 1 to compute the percentages of men awarded versus women awarded.
# What is the percentage of men awarded?

award_rate_men <- funding_rates_2by2$awarded[1]/(funding_rates_2by2$awarded[1] + funding_rates_2by2$not_awarded[1])
awarded_men/applications_men
award_rate_men

award_rate_women <- funding_rates_2by2$awarded[2]/(funding_rates_2by2$awarded[2] + funding_rates_2by2$not_awarded[2])
award_rate_women

# Question 3

# Run a chi-squared test External link on the two-by-two table to determine whether the difference in the two success rates is significant. (You can use tidy() to turn the output of chisq.test() into a data frame as well.)
# What is the p-value of the difference in funding rate?
tidy(funding_rates_2by2 %>% select(-gender) %>% chisq.test())

# Question 4

# There may be an association between gender and funding. But can we infer causation here? Is gender bias causing this observed difference? The response to the original paper claims that what we see here is similar to the UC Berkeley admissions example. Specifically they state that this "could be a prime example of Simpson’s paradox; if a higher percentage of women apply for grants in more competitive scientific disciplines, then an analysis across all disciplines could incorrectly show 'evidence' of gender inequality."

# To settle this dispute, use this dataset with number of applications, awards, and success rate for each gender:

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

dat %>%
  ggplot(aes(discipline, success, size=applications, color=gender)) + 
    geom_point() + 
    theme(axis.text.x = element_text(angle = 45, hjust=1))



