

# Load the required packages
library(tidyverse)
library(timetk)

# Load the data in a tibble
weather <- read_delim("data/raw/Deel_airport_weather_data/etmgeg_275_without_metadata.txt", 
                      delim = ",") 

# Convert the YYYYMMDD column to a R date object and rename to "date"

weather <- weather %>% 
  mutate(date = as.Date(as.character(YYYYMMDD), format = "%Y%m%d")) %>% 
  select(-YYYYMMDD) %>% 
  select(date, everything())

# Remove spaces in the titles of the columns

weather <- weather %>%
  rename_with(~ trimws(.x))

# Replace empty values with NAs

weather <- weather %>%
  mutate(across(where(is.character), ~ na_if(trimws(.x), "")))


# Do some exploration

summary <- weather %>% 
  drop_na(UG) %>% 
  summarize_by_time(date, 
                    .by = "month",
                    mean_humidity = mean(UG, drop_na = T))

