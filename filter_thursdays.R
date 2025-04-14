library(imager)
library(tidyverse)
library(phenopix)

# List of site names
sites <- c("H5R0-01", "H5R0-02", "H5R0-03", "H5R0-04")

# Loop over each site
for (site in sites) {
  # 1. Path to image folder for current site
  site_path <- file.path("data/raw/data/", site)
  
  # 2. List all JPG images in the folder
  im <- list.files(site_path, pattern = "\\.jpg$", full.names = TRUE, ignore.case = TRUE)
  
  if (length(im) == 0) {
    cat("No images found for", site, "\n")
    next
  }
  
  # 3. Extract just the filenames
  file_names <- basename(im)
  
  # 4. Extract date portion of filename (first 10 characters)
  date_strings <- substr(file_names, 1, 10)
  
  # 5. Convert to Date objects
  dates <- as.Date(date_strings, format = "%Y-%m-%d")
  
  # 6. Identify which files are from Thursdays
  is_thursday <- weekdays(dates) == "Thursday"
  thursday_files <- im[is_thursday]
  
  # 7. Create output folder for this site's Thursday images
  out_dir <- file.path("data/raw/data/", paste0(site, "_thursdays"))
  dir.create(out_dir, showWarnings = FALSE)
  
  # 8. Copy the Thursday images to the new folder
  file.copy(from = thursday_files,
            to = file.path(out_dir, basename(thursday_files)))
  
  cat("✅ Copied", length(thursday_files), "Thursday files for", site, "\n")
}
