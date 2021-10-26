options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)


titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic_known_age <- titanic %>%
  filter(!is.na(Age))

titanic_known_age %>% 
  ggplot(aes(Age, ..count.., fill=Sex)) +
  geom_density(alpha=0.6)

# stacked/facet
titanic_known_age %>% 
  ggplot(aes(Age)) +
  geom_density() +
  facet_grid(Sex ~ .)

# with variable counts
titanic_known_age %>% 
  ggplot(aes(Age, ..count.., fill=Sex)) +
  geom_density() +
  facet_grid(Sex ~ .)

titanic_known_age %>%
  count(Sex)

titanic_known_age %>%
  mutate(ageless17=Age < 17,age18to35= Age>=18 & Age <=35) %>%
  group_by(Sex) %>%
  summarize(ageless17rate=sum(ageless17, na.rm = TRUE)/n(), age18to35rate=sum(age18to35, na.rm = TRUE)/n(), total=n())

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic_known_age %>% 
  ggplot(aes(sample = Age)) +
  geom_qq(dparams=params) +
  geom_abline(slope=1, intercept = 0)

titanic %>%
  ggplot(aes(Survived)) +
  geom_bar(aes(fill = Sex)) 

titanic %>%
  ggplot(aes(Survived)) +
  geom_bar(aes(fill = Sex), position=position_dodge()) 

titanic %>%
  ggplot(aes(Sex)) +
  geom_bar(aes(fill = Survived), position=position_dodge()) 

titanic_known_age %>% 
  ggplot(aes(Age, ..count.., fill=Survived)) +
  geom_density(alpha=0.2)

titanic_known_fares <- titanic %>%
  filter(!is.na(Fare) & Fare > 0)

# Boxplot of Fare paid categorized by Survival
titanic_known_fares %>% 
  ggplot(aes(Survived, Fare, color=Survived)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  geom_jitter(alpha = 0.2, width = .2) +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +
  theme(legend.position = "none") 

titanic %>%
  ggplot(aes(Pclass, fill=Survived)) +
  geom_bar()

titanic %>%
  ggplot(aes(Pclass)) +
  geom_bar(aes(fill = Survived), position=position_fill())

titanic %>%
  ggplot(aes(Survived)) +
  geom_bar(aes(fill = Pclass), position=position_fill())

titanic_known_age %>% 
  ggplot(aes(Age, ..count.., fill=Survived)) +
  geom_density(alpha=0.6) +
  facet_grid(Sex ~ Pclass)
