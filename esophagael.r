library(tidyverse)
data("esoph")

head(esoph)

nrow(esoph)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)
esoph[which.max(esoph$tobgp),]
max(esoph$alcgp)

# alcohol
esoph %>% filter(alcgp == max(esoph$alcgp)) %>% summarize(tcases=sum(ncases),
                                               tcontrols=sum(ncontrols),
                                               prob=sum(ncases)/(sum(ncases)+sum(ncontrols)))
# alcohol min
esoph %>% filter(alcgp == min(esoph$alcgp)) %>% summarize(tcases=sum(ncases),
                                                          tcontrols=sum(ncontrols),
                                                          prob=sum(ncases)/(sum(ncases)+sum(ncontrols)))

# tabacco
esoph %>% filter(tobgp == '30+') %>% summarize(tcases=sum(ncases),
                                               tcontrols=sum(ncontrols),
                                               prob=sum(ncases)/(sum(ncases)+sum(ncontrols)))

# 
heavy_summary <- esoph %>%
  mutate(tobheavy = tobgp >= '10-19') %>%
  group_by(tobheavy) %>%
  summarize(tcases=sum(ncases), tcontrols=sum(ncontrols)) 

heavy_summary %>%
  summarize(propheavycase=heavy_summary[2,2]/sum(tcases))

heavy_summary %>%
  summarize(propheavycontrol=heavy_summary[2,3]/sum(tcontrols))

alc_summary <- esoph %>%
  group_by(alcgp) %>%
  summarize(tcases=sum(ncases),tcontrols=sum(ncontrols)) 

# probability of being in top alc group
top_alc_case_prop <- alc_summary[4, 2]/all_cases
top_alc_control_prop <- alc_summary[4, 3]/all_controls

top_alc_case_prop/top_alc_control_prop

tob_summary <- esoph %>%
  group_by(tobgp) %>%
  summarize(tcases=sum(ncases),tcontrols=sum(ncontrols)) 

# probability of being in top tob group
tob_summary$tcases[4]/all_cases
tob_summary$tcontrols[4]/all_controls

# probability of being in top alc & tob group
tob_alc_summary <- esoph %>%
  group_by(alcgp, tobgp) %>%
  summarize(tcases=sum(ncases),tcontrols=sum(ncontrols)) %>%
  filter(alcgp == max(esoph$alcgp) & tobgp == max(esoph$tobgp))

tob_alc_summary$tcases[1]/all_cases
tob_alc_summary$tcontrols[1]/all_controls

# probability of being in top alc or tob group
tob_alc_summary <- esoph %>%
  filter(alcgp == max(esoph$alcgp) | tobgp == max(esoph$tobgp)) %>%
  summarize(tcases=sum(ncases),tcontrols=sum(ncontrols)) 

tob_alc_summary$tcases[1]/all_cases
tob_alc_summary$tcontrols[1]/all_controls

(tob_alc_summary$tcases[1]/all_cases)/(tob_alc_summary$tcontrols[1]/all_controls)
