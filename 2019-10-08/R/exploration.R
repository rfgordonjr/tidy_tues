## Explore powerlifting dataset. ####
library(tidyverse)
library(here)
library(Hmisc)
library(gghighlight)

ipf_data <- readRDS(file = here::here('data', 'ipf_lifts.rds'))
str(ipf_data)

## Find date range of events ####
range(ipf_data$date)

## Can events last multiple days? Looks like they're all 1-day events. ####
ipf_data %>% 
  select(name, meet_name, date) %>% 
  mutate(yearDate = lubridate::year(date)) %>% 
  group_by(name, meet_name, yearDate) %>% 
  summarise(distinctDates = n_distinct(date)) %>% 
  ungroup() %>% 
  arrange(desc(distinctDates))

## How many unique names are there? #### 
ipf_data %>% 
  select(name) %>% 
  distinct() %>% 
  nrow()

## Which competitors have entered the most events? ####
ipf_data %>% 
  group_by(name) %>% # nrow()
  summarise(freq = n()) %>% 
  ungroup() %>% 
  arrange(desc(freq))
ipf_data %>% 
  group_by(name) %>% # nrow()
  summarise(freq = n()) %>% 
  ungroup() %>% 
  arrange(desc(freq)) %>% 
  slice(1:20) %>% 
  ggplot(.) +
  geom_bar(aes(reorder(name, -freq), freq), stat = "identity") +
  # theme(axis.text.x = element_text(angle = 90)) _
  theme(axis.text.x=element_text(size=12, angle=90,hjust=0.95,vjust=0.2)) +
  labs(title = "Top 20 Most Frequent Participants",
       subtitle = paste0(format(min(ipf_data$date), "%B %d, %Y"), " - ", format(max(ipf_data$date), "%B %d, %Y")),
       x = "Name",
       y = "Number of Events")

## Repeat but include by sex ####
ipf_data %>% 
  group_by(name, sex) %>% # nrow()
  summarise(freq = n()) %>% 
  arrange(sex, desc(freq)) %>%
  ungroup() %>% 
  group_by(sex) %>% 
  mutate(rowNum = row_number()) %>% 
  ungroup() %>% 
  filter(rowNum <= 20) 
ipf_data %>% 
  group_by(name, sex) %>% # nrow()
  summarise(freq = n()) %>% 
  arrange(sex, desc(freq)) %>%
  ungroup() %>% 
  group_by(sex) %>% 
  mutate(rowNum = row_number()) %>% 
  ungroup() %>% 
  filter(rowNum <= 20) %>% 
  ggplot(.) +
  geom_bar(aes(reorder(name, -freq), freq, fill = sex), stat = "identity") +
  # theme(axis.text.x = element_text(angle = 90)) _
  theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
  labs(title = "Top 20 Most Frequent Participants by Sex",
       subtitle = paste0(format(min(ipf_data$date), "%B %d, %Y"), " - ", format(max(ipf_data$date), "%B %d, %Y")),
       x = "Name",
       y = "Number of Events")

## Use facet instead ####
ipf_data %>% 
  group_by(name, sex) %>% # nrow()
  summarise(freq = n()) %>% 
  arrange(sex, desc(freq)) %>%
  ungroup() %>% 
  group_by(sex) %>% 
  mutate(rowNum = row_number()) %>% 
  ungroup() %>% 
  filter(rowNum <= 20) %>% 
  ggplot(.) +
  geom_bar(aes(reorder(name, -freq), freq, fill = sex), stat = "identity") +
  facet_grid(~sex, scales = "free_x") +
  # theme(axis.text.x = element_text(angle = 90)) _
  theme(axis.text.x=element_text(size=10, angle=90,hjust=0.95,vjust=0.2)) +
  labs(title = "Top 20 Most Frequent Participants by Sex",
       subtitle = paste0(format(min(ipf_data$date), "%B %d, %Y"), " - ", format(max(ipf_data$date), "%B %d, %Y")),
       x = "Name",
       y = "Number of Events")  

## Have average event numbers improved over time? ####
ipf_data %>% 
  mutate(yearDate = lubridate::year(date),
         monthDate = lubridate::month(date)) %>% 
  select(meet_name, yearDate, monthDate, best3bench_kg, best3deadlift_kg, best3squat_kg) %>% 
  gather(exercise, value, -c(meet_name, yearDate, monthDate)) %>% 
  # group_by(meet_name, yearDate, monthDate, exercise) %>% 
  group_by(yearDate, monthDate, exercise) %>% 
  summarise(medValue = median(value, na.rm=TRUE),
            meanValue = mean(value, na.rm=TRUE),
            sdValue = sd(value, na.rm=TRUE),
            numMeasurements = n()
            ) %>% 
  ungroup() %>% 
  arrange(exercise, yearDate, monthDate) %>% 
  mutate(approxDate = as.Date(paste0(yearDate, "-", monthDate, "-", "01"), "%Y-%m-%d"))
ipf_data %>% 
  mutate(yearDate = lubridate::year(date),
         monthDate = lubridate::month(date)) %>% 
  select(meet_name, yearDate, monthDate, best3bench_kg, best3deadlift_kg, best3squat_kg) %>% 
  gather(exercise, value, -c(meet_name, yearDate, monthDate)) %>% 
  # group_by(meet_name, yearDate, monthDate, exercise) %>% 
  group_by(yearDate, monthDate, exercise) %>% 
  summarise(medValue = median(value, na.rm=TRUE),
            meanValue = mean(value, na.rm=TRUE),
            sdValue = sd(value, na.rm=TRUE),
            numMeasurements = n()
  ) %>% 
  ungroup() %>% 
  arrange(exercise, yearDate, monthDate) %>% 
  mutate(approxDate = as.Date(paste0(yearDate, "-", monthDate, "-", "01"), "%Y-%m-%d")) %>% # describe()
  ggplot(.) +
  geom_line(aes(approxDate, medValue, col = exercise)) +
  geom_point(aes(approxDate, medValue, col = exercise))

## above plot probably shows lots of variance because of differences in sex ####
ipf_data %>% 
  mutate(yearDate = lubridate::year(date),
         monthDate = lubridate::month(date)) %>% 
  select(sex, meet_name, yearDate, monthDate, best3bench_kg, best3deadlift_kg, best3squat_kg) %>% 
  gather(exercise, value, -c(sex, meet_name, yearDate, monthDate)) %>% 
  # group_by(sex, meet_name, yearDate, monthDate, exercise) %>% 
  group_by(sex, yearDate, monthDate, exercise) %>% 
  summarise(medValue = median(value, na.rm=TRUE),
            meanValue = mean(value, na.rm=TRUE),
            sdValue = sd(value, na.rm=TRUE),
            numMeasurements = n()
  ) %>% 
  ungroup() %>% 
  arrange(exercise, yearDate, monthDate) %>% 
  mutate(approxDate = as.Date(paste0(yearDate, "-", monthDate, "-", "01"), "%Y-%m-%d")) %>% # View()
  ggplot(.) +
  geom_line(aes(approxDate, medValue, col = exercise)) +
  geom_point(aes(approxDate, medValue, col = exercise)) +
  geom_smooth(aes(approxDate, medValue, col = exercise)) +
  facet_grid(exercise~sex) +
  labs(title = "Changes of Monthly Median Performance Over Time",
       subtitle = "Facetted by Sex and Exercise",
       x = "Month",
       y = "Median Value")
## Repeat by year ####
ipf_data %>% 
  mutate(yearDate = lubridate::year(date),
         monthDate = lubridate::month(date)) %>% 
  select(sex, meet_name, yearDate, monthDate, best3bench_kg, best3deadlift_kg, best3squat_kg) %>% 
  gather(exercise, value, -c(sex, meet_name, yearDate, monthDate)) %>% 
  mutate(exercise = case_when(exercise == "best3bench_kg" ~ "Bench Press",
                              exercise == "best3deadlift_kg" ~ "Dead Lift",
                              exercise == "best3squat_kg" ~ "Squat",
                              TRUE ~ exercise)
         ) %>% 
  # group_by(sex, meet_name, yearDate, monthDate, exercise) %>% 
  # group_by(sex, yearDate, monthDate, exercise) %>% 
  group_by(sex, yearDate, exercise) %>% 
  summarise(medValue = median(value, na.rm=TRUE),
            meanValue = mean(value, na.rm=TRUE),
            sdValue = sd(value, na.rm=TRUE),
            numMeasurements = n()
  ) %>% 
  ungroup() %>% 
  arrange(exercise, yearDate) %>% 
  # mutate(approxDate = as.Date(paste0(yearDate, "-", monthDate, "-", "01"), "%Y-%m-%d")) %>% # View()
  ggplot(.) +
  geom_line(aes(yearDate, medValue, col = exercise)) +
  geom_point(aes(yearDate, medValue, col = exercise)) +
  geom_smooth(aes(yearDate, medValue, col = exercise)) +
  facet_grid(exercise~sex) +
  labs(title = "Changes of Yearly Median Performance Over Time",
       subtitle = "Facetted by Sex and Exercise",
       x = "Year",
       y = "Median Value (kg)")

## What if we mark years where no one was found to be doping in that respective exercise? ####
sort(unique(ipf_data$place))
plot1 <- ipf_data %>% 
  mutate(yearDate = lubridate::year(date),
         monthDate = lubridate::month(date),
         indDoping = if_else(place == "DD", 1, 0)) %>% 
  select(sex, meet_name, indDoping, yearDate, monthDate, best3bench_kg, best3deadlift_kg, best3squat_kg) %>% 
  gather(exercise, value, -c(sex, meet_name, indDoping, yearDate, monthDate)) %>% 
  mutate(exercise = case_when(exercise == "best3bench_kg" ~ "Bench Press",
                              exercise == "best3deadlift_kg" ~ "Dead Lift",
                              exercise == "best3squat_kg" ~ "Squat",
                              TRUE ~ exercise)
  ) %>% 
  # group_by(sex, meet_name, yearDate, monthDate, exercise) %>% 
  # group_by(sex, yearDate, monthDate, exercise) %>% 
  group_by(sex, yearDate, exercise) %>% 
  summarise(medValue = median(value, na.rm=TRUE),
            meanValue = mean(value, na.rm=TRUE),
            sdValue = sd(value, na.rm=TRUE),
            numMeasurements = n(),
            hasDoping = max(indDoping, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  arrange(exercise, yearDate) %>% # View()
  # mutate(approxDate = as.Date(paste0(yearDate, "-", monthDate, "-", "01"), "%Y-%m-%d")) %>% # View()
  ggplot(.) +
  # geom_line(aes(yearDate, medValue, col = exercise)) +
  geom_point(aes(yearDate, medValue, col = exercise)) +
  # geom_smooth(aes(yearDate, medValue, col = exercise)) +
  gghighlight(hasDoping == 0, use_direct_label = FALSE, use_facet_vars = TRUE) +
  facet_grid(exercise~sex) +
  labs(title = "Yearly Median (Clean) Performance Over Time",
       subtitle = "Grey Points Include at Least 1 DQ Event for Doping",
       x = "Year",
       y = "Median Value (kg)") +
  theme_bw()
ggsave(filename = 'medPerfCheckDoping.png',path = here::here('plots'))
