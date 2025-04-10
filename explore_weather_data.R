

# Load the required packages
library(tidyverse)

# Load the data in a tibble
weather <- read_delim("data/raw/Deel_airport_weather_data/etmgeg_275_without_metadata.txt", 
                      delim = ",") 

# Convert the YYYYMMDD column to a R date object

weather <- weather %>% 
  mutate(YYYYMMDD = as.Date(as.character(YYYYMMDD), format = "%Y%m%d"))


