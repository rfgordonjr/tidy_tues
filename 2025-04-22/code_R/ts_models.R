## Hypothesis: previous day of 4/20 is a significant predictor of accidents on 4/21 ####
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

accidents_420 <- readr::read_csv(file = here::here('dat/daily_accidents_420.csv'))
format(max(accidents_420$date), "%Y/%m/%d")
ymd(format(max(accidents_420$date), "%Y/%m/%d")) - days(3)

## Check if date is 420
is420 = function(x){
  lubridate::month(x)==4 & lubridate::day(x)==20
}

## 4/20s are doubled for e420 in (TRUE, FALSE), fix for lagged vars
dat5yr <- accidents_420 %>% 
  mutate(e420 = replace_na(e420, FALSE)) %>% 
  group_by(date, e420) %>% 
  summarise(fatalities_count = sum(fatalities_count)) %>% 
  ungroup() %>% 
  filter(date > ymd(format(max(date), "%Y/%m/%d")) - years(5)) %>% 
  arrange(date) %>% filter(date == as.Date("2015-04-20")) %>% View()
  mutate(fatal_lag1 = lag(fatalities_count, 1),
         fatal_lag2 = lag(fatalities_count, 2),
         fatal_lag3 = lag(fatalities_count, 3),
         fatal_lag4 = lag(fatalities_count, 4),
         fatal_lag5 = lag(fatalities_count, 5),
         fatal_lag6 = lag(fatalities_count, 6),
         fatal_lag7 = lag(fatalities_count, 7),
         date_lag1_420 = is420(lag(date, 1)),
         date_lag2_420 = is420(lag(date, 2)),
         date_lag3_420 = is420(lag(date, 3)),
         date_lag4_420 = is420(lag(date, 4)),
         date_lag5_420 = is420(lag(date, 5)),
         date_lag6_420 = is420(lag(date, 6)),
         date_lag7_420 = is420(lag(date, 7))
         )
dat5yr %>% 
  select(date, date_lag1_420:date_lag7_420) %>% 
  filter(date_lag1_420) %>% 
  View()
