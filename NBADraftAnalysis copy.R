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

nonlotterypicksind <- data %>% 
  filter(PickType != 'Lottery') %>% 
  group_by(Pk) %>% 
  summarise(across(c(MPG, PPG, APG, RPG, BPM), list(mean = ~ round(mean(.), 1)),
                   .names = "{col}")
  ) %>% 
  gt()

gtsave(nonlotterypicksind, 'Non-LotteryPicks-Stats-Ind.png')

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

threept <- data %>% 
  filter(!is.na(`3P%`)) %>% 
  group_by(DraftYr) %>%
  summarise(
    avg_3pt = mean(`3P%`)
  ) %>% 
  ggplot(aes(x = DraftYr, y = avg_3pt)) +
  geom_line(color = 'black') +
  labs(x = 'Draft Year', y = 'Average 3-Point Percentage') +
  theme_minimal()

ggsave('threept.png', plot = threept, width = 8, height = 6, dpi = 300, bg = "white")

lotterygames <- data %>% 
  filter(PickType == 'Lottery') %>% 
  ggplot(aes(x = G)) +
  geom_histogram(fill = 'blue') +
  labs(x = 'Games', y = 'Frequency') +
  theme_minimal()

ggsave('lottery-games-played.png', plot = lotterygames, width = 8, height = 6,
       dpi = 300, bg = 'white')

nonlotgames <- data %>% 
  filter(PickType != "Lottery") %>% 
  ggplot(aes(x = G)) +
  geom_histogram(fill = 'blue') +
  labs(x = 'Games', y = 'Frequency') +
  theme_minimal()

ggsave('nonlottery-games-played.png', plot = nonlotgames, width = 8, height = 6,
       dpi = 300, bg = 'white')

summarystats <- data %>% 
  group_by(DraftYr) %>% 
  summarise(
    PPG = mean(PPG),
    APG = mean(APG),
    RPG = mean(RPG)
  )
long_data <- summarystats %>% 
  pivot_longer(cols = c(PPG, APG, RPG), names_to = 'Stats', values_to = 'Value')

statsplot <- ggplot(long_data, aes(x = DraftYr, y = Value, color = Stats)) +
  geom_line() +
  labs(title = 'Points, Assists, and Rebounds Per Game Over Time', x = 'Year') +
  theme_minimal()

ggsave('summarystats.png', plot = statsplot, width = 8, height = 6,
       dpi = 300, bg = 'white')

bpm_over_time <- data %>% 
  group_by(DraftYr) %>% 
  summarise(BPM = mean(BPM)) %>% 
  ggplot(aes(x = DraftYr, y = BPM)) +
  geom_line() +
  labs(title = 'Box Plus-Minus Over Time', x = 'Year') +
  theme_minimal()

ggsave('bpm-over-time.png', plot = bpm_over_time, width = 8, height = 6, dpi = 300,
       bg = 'white')

internationalvorp <- data %>% 
  filter(College == 'International/High School') %>% 
  group_by(DraftYr) %>% 
  summarise(VORP = mean(VORP)) %>% 
  ggplot(aes(x = DraftYr, y = VORP)) +
  geom_line() +
  labs(title = 'Foreign Player VORP over Time', x = 'Year') +
  theme_minimal()

ggsave('international-hs-vorp.png', plot = internationalvorp, width = 8, height = 6,
       dpi = 300, bg = 'white')

bestdraftclasses <- data %>% 
  filter(DraftYr %in% c(1996, 2003))

bestsummarystats <- bestdraftclasses %>% 
  group_by(DraftYr) %>% 
  summarise(
    Yrs = mean(Yrs),
    PPG = mean(PPG),
    APG = mean(APG),
    RPG = mean(RPG)
  )

best_summary_long <- bestsummarystats %>% 
  pivot_longer(cols = c(Yrs, PPG, APG, RPG), names_to = 'Stats', values_to = 'Value')

best_summary_long$DraftYr <- as.character(best_summary_long$DraftYr)

allotheryears <- data %>% 
  filter(!DraftYr %in% c(1996, 2003))

allotheryears_summary <- allotheryears %>% 
  summarise(
    DraftYr = 'Other Years',
    Yrs = mean(Yrs),
    PPG = mean(PPG),
    APG = mean(APG),
    RPG = mean(RPG)
  )

other_long <- allotheryears_summary %>% 
  pivot_longer(cols = c(Yrs, PPG, APG, RPG), names_to = 'Stats', values_to = 'Value')

combined_data <- bind_rows(best_summary_long, other_long)

combined_summary <- combined_data %>% 
  ggplot(aes(x = factor(DraftYr), y = Value, fill = Stats)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = 'Draft Year') +
  scale_fill_manual(values = c('APG' = 'red', 'PPG' = 'blue', 'RPG' = 'green', 'Yrs' = 'purple')) +
  theme_minimal()

ggsave('combined-summary.png', plot = combined_summary, width = 8, height = 6,
       dpi = 300, bg = 'white')  

data %>% 
  filter(PickType != 'Lottery') %>% 
  select(Player, BPM) %>% 
  arrange(desc(BPM)) %>% 
  head(25) %>% 
  print(n = 25)

data %>% 
  filter(PickType == 'Lottery') %>% 
  group_by(Pk) %>% 
  summarise(
    BPM = mean(BPM)
  )

data %>% 
  filter(Pk == 9) %>% 
  select(Player, VORP) %>% 
  arrange(desc(VORP)) %>% 
  head(25) %>% 
  print(n = 25)

data %>% 
  filter(Pk == 13) %>% 
  select(Player, VORP) %>% 
  arrange(desc(VORP)) %>% 
  head(25) %>% 
  print(n = 25)

View(data)


data %>% 
  filter(DraftYr == 1997) %>%
  filter(College == 'International/High School') %>% 
  arrange(desc(VORP)) %>% 
  select(Player, Pk, VORP, DraftYr)

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
