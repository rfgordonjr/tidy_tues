library(here)
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

accidents <- readr::read_csv(file = here::here('dat/daily_accidents.csv'))
accidents_420 <- readr::read_csv(file = here::here('dat/daily_accidents_420.csv'))
accidents_420_t <- readr::read_csv(file = here::here('dat/daily_accidents_420_time.csv'))

## Simple plot ####
ggplot(accidents, aes(date, fatalities_count)) +
  geom_line()

## filter for last 3 years of data set to see more easliy ####
ymd(format(max(accidents$date), "%Y/%m/%d")) - years(3)
accidents %>% 
  filter(date >= ymd(format(max(date), "%Y/%m/%d")) - years(3)) %>% 
  ggplot(aes(date, fatalities_count)) +
  geom_line()

## replace lines with dots, color by "occur on 420" ####
accidents_420 %>% 
  mutate(e420 = replace_na(e420, FALSE)) %>% 
  filter(date >= ymd(format(max(date), "%Y/%m/%d")) - years(3)) %>% 
  filter(lubridate::month(date)==4, lubridate::day(date)==20)
## Let's assume missing e420 is a FALSE 
accidents_420 %>% 
  mutate(e420 = replace_na(e420, FALSE)) %>% 
  group_by(date, e420) %>% 
  summarise(fatalities_count = sum(fatalities_count)) %>% 
  ungroup() %>% 
  filter(date >= ymd(format(max(date), "%Y/%m/%d")) - years(3)) %>% 
  ggplot(aes(date, fatalities_count, col = e420)) +
  geom_point()

## are 420 accidents consistently lower like they are for the last 3 years? ####
accidents_420 %>% 
  mutate(e420 = replace_na(e420, FALSE)) %>% 
  group_by(date, e420) %>% 
  summarise(fatalities_count = sum(fatalities_count)) %>% 
  ungroup() %>% 
  # filter(date >= ymd(format(max(date), "%Y/%m/%d")) - years(3)) %>% 
  ggplot(aes(date, fatalities_count, col = e420, alpha = 0.3)) +
  geom_point()

accidents_420 %>% 
  mutate(e420 = replace_na(e420, FALSE)) %>% 
  group_by(date, e420) %>% 
  summarise(fatalities_count = sum(fatalities_count)) %>% 
  ungroup() %>% 
  group_by(e420) %>% 
  mutate(mean_deaths = mean(fatalities_count),
            median_deaths = median(fatalities_count)) %>% 
  ungroup() %>% 
  # filter(date >= ymd(format(max(date), "%Y/%m/%d")) - years(3)) %>% 
  ggplot() +
  geom_point(aes(date, fatalities_count, col = e420, alpha = 0.3)) +
  geom_line(aes(date, fatalities_count, col = e420, alpha = 0.3))
  # geom_hline(aes(yintercept = mean_deaths, lty = e420))
