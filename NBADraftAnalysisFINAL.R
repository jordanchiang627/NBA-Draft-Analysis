library(tidyverse)
library(gt)

data <- read_delim('draft-data-20-years.csv')

lotterypicks <- data %>% 
  filter(PickType == 'Lottery') %>% 
  group_by(Pk) %>% 
  summarise(
    across(c(MPG, PPG, APG, RPG, BPM), list(mean = ~ round(mean(.), 1)),
           .names = "{col}")
  ) %>% 
  gt()

gtsave(lotterypicks, 'Lottery_Pick_Stats.png')

nonlotterypicks <- data %>% 
  filter(PickType != 'Lottery') %>% 
  summarise(across(c(MPG, PPG, APG, RPG, BPM), list(mean = ~ round(mean(.), 1)),
                   .names = "{col}")
  ) %>% 
  gt()

gtsave(nonlotterypicks, 'Non-LotteryPicks-Stats.png')

lotteryvorp <- data %>% 
  filter(PickType == 'Lottery') %>% 
  group_by(Pk) %>% 
  summarise(
    VORP = mean(VORP)
  ) %>% 
  ggplot(aes(x = factor(Pk), y = VORP)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  labs(x = 'Pick', y = 'Value Over Replacement Player') +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 30))
ggsave("lottery_vorp.png", lotteryvorp, width = 8, height = 6, dpi = 300, bg = "white")

colleges <- data %>% 
  filter(College != 'International/High School', PickType == 'Lottery') %>% 
  count(College) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  gt()

gtsave(colleges, 'lottery-colleges.png')

data %>% 
  filter(College == 'Syracuse', PickType == 'Lottery') %>% 
  select(Player, College, Pk, DraftYr)

PickTypeVORP <- data %>% 
  group_by(PickType) %>% 
  summarise(
    VORP = mean(VORP)
  ) %>% 
  ggplot(aes(PickType, VORP)) +
  geom_bar(stat = "identity", fill ='blue') +
  labs(x = 'Pick Type') +
  theme_minimal()

ggsave('PickTypeVORP.png', plot = PickTypeVORP, width = 4, height = 6,
       dpi = 300, bg = 'white') 
