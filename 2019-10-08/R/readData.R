## Load Packages ####
library(tidyverse)
library(janitor)
library(here)

## Pull Data ####
ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")
str(ipf_lifts)

## Follow rest of cleaning script ####
# df <- read_csv(here::here("openpowerlifting-2019-09-20", "openpowerlifting-2019-09-20.csv"))

df_clean <- ipf_lifts %>% 
  janitor::clean_names()

df_clean %>% 
  group_by(federation) %>% 
  count(sort = TRUE)

size_df <- df_clean %>% 
  select(name:weight_class_kg, starts_with("best"), place, date, federation, meet_name)  %>% 
  filter(!is.na(date)) %>% 
  filter(federation == "IPF") %>% 
  object.size()

ipf_data <- df_clean %>% 
  select(name:weight_class_kg, starts_with("best"), place, date, federation, meet_name)  %>% 
  filter(!is.na(date)) %>% 
  filter(federation == "IPF")

print(size_df, units = "MB")

# ipf_data %>% 
#   write_csv(here::here("2019", "2019-10-08","ipf_lifts.csv"))
ipf_data %>% 
  write_csv(here::here('data', 'ipf_lifts.csv'))
saveRDS(object = ipf_data, file = here::here('data', 'ipf_lifts.rds'))
