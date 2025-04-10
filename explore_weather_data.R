

# Load the required packages
library(tidyverse)
library(timetk)

# Load the data in a tibble
weather <- read_delim("data/raw/Deel_airport_weather_data/etmgeg_275_without_metadata.txt", 
                      delim = ",") 

# Convert the YYYYMMDD column to a R date object and rename to date

weather <- weather %>% 
  mutate(date = as.Date(as.character(YYYYMMDD), format = "%Y%m%d")) %>% 
  select(-YYYYMMDD) %>% 
  select(date, everything())


# Do some exploration

weather %>% 
  group_by(UG) %>% 
  summarize_by_time()

