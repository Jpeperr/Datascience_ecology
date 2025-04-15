

# Load the required packages
library(tidyverse) #
library(timetk)
library(tidymodels)
library(writexl)
library(phenofit)
library(bfast)
library(phenocamr)
library(phenopix)
library(imager)
library(zoo)
library(tools)

# Load the data in a tibble
setwd("C:/Users/Luuk/OneDrive - 
      Wageningen University & Research/Master Jaar 1/7. 
      Data Science for Ecology/Challenge/Datascience_ecology") # Set you own WD
weather <- read_delim("data/raw/Deel_airport_weather_data/etmgeg_275_without_metadata.txt", 
                      delim = ",") 

# Data wrangling on raw data
##############################################################################################

# Convert the YYYYMMDD column to a R date object and rename to "date"

weather <- weather %>% 
  mutate(date = as.Date(as.character(YYYYMMDD), format = "%Y%m%d")) %>% 
  select(-YYYYMMDD) %>% 
  select(date, everything())


# Split the YYYYMMDD columns into 
# "year", "month", "day", "quarter and "season" for later use
weather <- weather %>% 
  mutate(year = year(date), month = month(date), day = day(date), 
         quarter = quarter(date)) %>% 
  mutate(
    season = case_when(
      month %in% 3:5 ~ "Spring",
      month %in% 6:8 ~ "Summer",
      month %in% 9:11 ~ "Autumn", 
      month %in% c(12, 1, 2) ~ "Winter" 
    )) %>% 
  select(date, year, month, day, quarter, season, everything())
  


# Remove spaces in the titles of the columns

weather <- weather %>%
  rename_with(~ trimws(.x))

# Replace empty values with NAs

weather <- weather %>%
  mutate(across(where(is.character), ~ na_if(trimws(.x), "")))

# Making the variables numeric expect the dates

weather <- weather %>%
  mutate(across(
    .cols = -c(date, month, year, day, quarter, season),
    .fns = ~ as.numeric(.)
  )) 

# Convert some variables to appropriate units

weather <- weather %>%
  mutate(
    TG = TG / 10,   # convert 0.1 °C to °C
    TN = TN / 10,
    TX = TX / 10,
    RH = ifelse(RH == -1, NA, RH / 10) # RH: set -1 to NA, then scale to mm
  )


# Making new weather variabls using WorldClim weather statistics
##############################################################################################


# 1. Summarize per month
monthly_weather <- weather %>%
  group_by(year, month) %>%
  summarize(
    tmean_month = mean(TG, na.rm = TRUE),
    tmin_month = mean(TN, na.rm = TRUE),
    tmax_month = mean(TX, na.rm = TRUE),
    tmax_daily_max = max(TX, na.rm = TRUE),
    tmin_daily_min = min(TN, na.rm = TRUE),
    prec_month = sum(RH, na.rm = TRUE),
    .groups = 'drop'
  )

# 2. Summarize per quarter
quarterly_weather <- weather %>%
  group_by(year, quarter) %>%
  summarize(
    tmean_quarter = mean(TG, na.rm = TRUE),
    prec_quarter = sum(RH, na.rm = TRUE),
    .groups = 'drop'
  )

# 3. Summarize per year
annual_weather <- weather %>%
  group_by(year) %>%
  summarize(
    BIO1 = mean(TG, na.rm = TRUE), # Annual Mean Temperature
    BIO12 = sum(RH, na.rm = TRUE), # Annual Precipitation
    BIO4 = sd(TG, na.rm = TRUE) * 100, # Temperature Seasonality (standard deviation ×100)
    BIO15 = (sd(RH, na.rm = TRUE) / mean(RH, na.rm = TRUE)) * 100, # Precipitation Seasonality (Coefficient of Variation)
    .groups = 'drop'
  )

# 4. Calculate other BIO variables

# BIO2: Mean Diurnal Range (Mean of monthly (max temp - min temp))
BIO2_data <- monthly_weather %>%
  mutate(diurnal_range = tmax_month - tmin_month) %>%
  group_by(year) %>%
  summarize(BIO2 = mean(diurnal_range, na.rm = TRUE), .groups = 'drop')

# BIO5 and BIO6: Max Temp of Warmest Month & Min Temp of Coldest Month
BIO5_6_data <- monthly_weather %>%
  group_by(year) %>%
  summarize(
    BIO5 = max(tmax_daily_max, na.rm = TRUE), # Max Temperature of Warmest Month
    BIO6 = min(tmin_daily_min, na.rm = TRUE), # Min Temperature of Coldest Month
    .groups = 'drop'
  )

# BIO7: Temperature Annual Range (BIO5 - BIO6)
BIO7_data <- BIO5_6_data %>%
  mutate(BIO7 = BIO5 - BIO6)

# BIO3: Isothermality (BIO2 / BIO7 * 100)
BIO3_data <- BIO2_data %>%
  inner_join(BIO7_data, by = "year") %>%
  mutate(BIO3 = (BIO2 / BIO7) * 100) %>%
  select(year, BIO3)

# BIO8 - BIO11: Mean Temp of Wettest, Driest, Warmest, Coldest Quarter
wettest_driest_quarters <- quarterly_weather %>%
  group_by(year) %>%
  summarize(
    wettest_quarter = quarter[which.max(prec_quarter)],
    driest_quarter = quarter[which.min(prec_quarter)],
    warmest_quarter = quarter[which.max(tmean_quarter)],
    coldest_quarter = quarter[which.min(tmean_quarter)],
    .groups = 'drop'
  )

BIO8_11_data <- quarterly_weather %>%
  inner_join(wettest_driest_quarters, by = "year") %>%
  group_by(year) %>%
  summarize(
    BIO8 = mean(tmean_quarter[quarter == wettest_quarter], na.rm = TRUE),
    BIO9 = mean(tmean_quarter[quarter == driest_quarter], na.rm = TRUE),
    BIO10 = mean(tmean_quarter[quarter == warmest_quarter], na.rm = TRUE),
    BIO11 = mean(tmean_quarter[quarter == coldest_quarter], na.rm = TRUE),
    .groups = 'drop'
  )

# BIO13 and BIO14: Precipitation of Wettest and Driest Month
BIO13_14_data <- monthly_weather %>%
  group_by(year) %>%
  summarize(
    BIO13 = max(prec_month, na.rm = TRUE),
    BIO14 = min(prec_month, na.rm = TRUE),
    .groups = 'drop'
  )

# BIO16 - BIO19: Precipitation of Wettest, Driest, Warmest, Coldest Quarter
BIO16_19_data <- quarterly_weather %>%
  inner_join(wettest_driest_quarters, by = "year") %>%
  group_by(year) %>%
  summarize(
    BIO16 = max(prec_quarter[quarter == wettest_quarter], na.rm = TRUE),
    BIO17 = min(prec_quarter[quarter == driest_quarter], na.rm = TRUE),
    BIO18 = max(prec_quarter[quarter == warmest_quarter], na.rm = TRUE),
    BIO19 = max(prec_quarter[quarter == coldest_quarter], na.rm = TRUE),
    .groups = 'drop'
  )

# Summarize mean temperature and sum of precipiation per season
seasonal_weather <- weather %>%
  group_by(year, season) %>%
  summarize(
    t.mean = mean(TG, na.rm = TRUE),
    prec.sum = sum(RH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = season,
    values_from = c(t.mean, prec.sum),
    names_glue = "{.value}_{season}"
  )


# 5. Merge everything together
BIO_summary <- annual_weather %>%
  left_join(BIO2_data, by = "year") %>%
  left_join(BIO3_data, by = "year") %>%
  left_join(BIO5_6_data, by = "year") %>%
  left_join(BIO7_data %>% select(year, BIO7), by = "year") %>%
  left_join(BIO8_11_data, by = "year") %>%
  left_join(BIO13_14_data, by = "year") %>%
  left_join(BIO16_19_data, by = "year") %>% 
  left_join(seasonal_weather, by = "year")

# 6. Remove the years before 2012 and after 2020
BIO_summary <- BIO_summary %>% 
  filter(year >= 2012, year <= 2020)



# 7. Export the table to excel
write_csv(BIO_summary, 
           file = "data/processed/BIO_clim_processed.csv")


# Combine with the phenophase metrics
##############################################################################################

# Load in the vegetation indexes
load("data/processed/vi_output/vi_output/H5R0-01/H5R0-01VI.Rdata")


vi <- VI.data$RO1 %>% 
  as.tibble() %>% 
  mutate(gcc = g.av / (r.av + g.av + b.av), 
         year = year(date), 
         date = as.Date(date)) %>% 
  distinct() %>% 
  drop_na(gcc) %>% 
  arrange(date) %>% 
  filter(year == 2015)


zoo <- zoo(vi$gcc,
           order.by = vi$date)

explored_RO1 <- greenExplore(zoo)

# Analysis
##############################################################################################

fake_data <- read_delim("data/processed/BIO_clim_processed_with_fake_data.csv")

ggplot(fake_data, aes(x = prec.sum_Spring, y = Greenup)) +
  geom_point()

lm <- lm(Greenup ~ prec.sum_Spring, data = fake_data)
plot(lm)
summary(lm)


# Visualizing some variables
##############################################################################################

# Making a summary table

summary <- weather %>% 
  mutate(UG = as.numeric(UG)) %>% 
  mutate(RH = as.numeric(RH)) %>% 
  mutate(TG = as.numeric(TG)) %>% 
  drop_na(UG, RH, TG) %>% 
  summarize_by_time(date, 
                    .by = "year",
                    mean_humidity = mean(UG, na.rm = T),
                    mean_rainfall = mean(RH, na.rm = T),
                    BIO1 = mean(TG, na.rm = T) /10)
  
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

# Visualize mean precipitation and temp per year

summary %>% plot_time_series(
  .date_var = date, mean_rainfall,
  .smooth = T
)

summary %>% plot_time_series(
  .date_var = date, BIO1,
  .smooth = T
)






