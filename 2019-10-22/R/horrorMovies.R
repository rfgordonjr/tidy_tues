library(here)
library(tidyverse)
library(ggTimeSeries)
library(leaflet)

# horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")
# saveRDS(object = horror_movies,file = here::here('horror_movies.rds'))
horror_movies <- readRDS(file = here::here('horror_movies.rds'))

## Are review ratings related to calendar release date? ####
head(horror_movies$release_date)
horror_movies$release_date <- as.Date(horror_movies$release_date, "%d-%b-%y")
horror_movies %>% 
  mutate(newDate = format(release_date, "%d/%m/%Y")) %>% 
  # group_by(release_date) %>% 
  group_by(newDate) %>% 
  summarise(minRating = min(review_rating, na.rm=TRUE),
            medRating = median(review_rating, na.rm=TRUE),
            meanRating = mean(review_rating, na.rm=TRUE),
            maxRating = max(review_rating, na.rm=TRUE)
  ) %>% 
  ungroup() %>% # View()
  ggplot_calendar_heatmap(dtDateValue = .,
                        # cDateColumnName = 'release_date',
                        cDateColumnName = 'newDate',
                        cValueColumnName = 'medRating')
