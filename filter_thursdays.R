library(imager)
library(tidyverse)
library(phenopix)

# filter over all images on mondays 
im <- list.files("data_challenges/data/raw_2/H5R0-01/", pattern = "\\.JPG$", full.names = TRUE)
head(im)

# 2. Extract the filename only (just the part like "2013-07-10-12h00m00.JPG")
file_names <- basename(im)

# 3. Extract the date part (first 10 characters of the filename)
date_strings <- substr(file_names, 1, 10)

# 4. Convert to Date class
dates <- as.Date(date_strings, format = "%Y-%m-%d")

# 5. Check which are Mondays
is_monday <- weekdays(dates) == "Monday"

# 6. Filter the paths to only keep Monday images
monday_files <- im[is_monday]

# Create a new folder to store Monday files
dir.create("data_challenges/data/monday_images", showWarnings = FALSE)

# Copy the Monday files to the new folder
file.copy(from = monday_files,
          to = file.path("data_challenges/data/monday_images", basename(monday_files)))
