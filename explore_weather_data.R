

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


# Split the YYYYMMDD columns into "year", "month" and "day"
weather <- weather %>% 
  mutate(year = year(date), month = month(date), day = day(date)) %>% 
  select(date, year, month, day, everything())


# Remove spaces in the titles of the columns

weather <- weather %>%
  rename_with(~ trimws(.x))

# Replace empty values with NAs

weather <- weather %>%
  mutate(across(where(is.character), ~ na_if(trimws(.x), "")))


# Summarize some weather statistics

summary <- weather %>% 
  mutate(UG = as.numeric(UG)) %>% 
  drop_na(UG, ) %>% 
  summarize_by_time(date, 
                    .by = "3 months",
                    mean_humidity = mean(UG, na.rm = T),
                    )
  

# Visualize mean humidity per 3 months

ggplot(summary, aes(x = date, y = mean_humidity)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  geom_smooth(stat = "smooth")


