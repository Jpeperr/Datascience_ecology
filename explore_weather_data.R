

# Load the required packages
library(tidyverse)
library(timetk)

# Load the data in a tibble
setwd("C:/Users/Luuk/OneDrive - Wageningen University & Research/Master Jaar 1/7. Data Science for Ecology/Challenge/Datascience_ecology")
weather <- read_delim("data/raw/Deel_airport_weather_data/etmgeg_275_without_metadata.txt", 
                      delim = ",") 

# Convert the YYYYMMDD column to a R date object and rename to "date"

weather <- weather %>% 
  mutate(date = as.Date(as.character(YYYYMMDD), format = "%Y%m%d")) %>% 
  select(-YYYYMMDD) %>% 
  select(date, everything())


# Split the YYYYMMDD columns into "year", "month" and "day" for later use
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
  mutate(RH = as.numeric(RH)) %>% 
  drop_na(UG, RH) %>% 
  summarize_by_time(date, 
                    .by = "year",
                    mean_humidity = mean(UG, na.rm = T),
                    mean_rainfall = mean(RH, na.rm = T))
  
# Visualize mean humidity per year

ggplot(summary, aes(x = date, y = mean_humidity)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  geom_smooth(stat = "smooth")

summary %>% plot_time_series(
  .date_var = date, mean_humidity,
  .smooth = T
)

# Visualize mean precipitation per year

summary %>% plot_time_series(
  .date_var = date, mean_rainfall,
  .smooth = T
)


# Making a model to compare humidity and rainfall

## First scaling the numeric variables
dat %>% 
  mutate(across(where(is.numeric), ~ (.x - mean(.x)) / sd(.x)))




