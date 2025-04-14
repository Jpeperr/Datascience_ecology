library(tidyverse)
library(lubridate)
library(zoo)
library(greenbrown) # assuming greenExplore and plotExplore are from this package
library(fs)         # for path()

sites <- c("H5R0-01")

for (site in sites) {
  # Build file path
  vi_file <- path("data/processed/vi_output", site, paste0(site, "VI.Rdata"))
  
  # Load .Rdata into a separate environment to access its contents cleanly
  e <- new.env()
  load(vi_file, envir = e)
  
  # Check structure
  str(e)
  
  # Assuming the loaded object is named 'RO1'
  VI.data <- as_tibble(e$RO1)
  
  # Plot time series of g.av
  VI.data %>%
    ggplot(aes(x = date, y = g.av)) +
    geom_line() +
    labs(title = paste("Green Chromatic Coordinate -", site))
  
  # Subset and order by date for 2019
  vi_season <- VI.data %>%
    filter(year(date) == 2019) %>%
    arrange(date)
  
  # Convert to zoo object
  gcc_zoo <- zoo(vi_season$g.av, order.by = vi_season$date)
  
  # Explore seasonality
  explored <- greenExplore(gcc_zoo)
  plotExplore(explored)
}

