# PACKAGES
auto_install_package <- function(package_names_list){
  for(package_name in package_names_list){
    if(!(package_name %in% rownames(installed.packages()))){
      install.packages(package_name, dependencies = TRUE)
    }
    library(package_name, character.only = TRUE)
  }
}

auto_install_package(c("tidyverse", "phenofit", "bfast", "phenocamr", "phenopix",
                       "imager", "tools", "zoo"))

### NOTE
#' script assumes a main data folder exists in the same location as the script.
#' "data" contains a "raw" folder (with raw image data) and a "processed" folder to store results.

# 🔁 Loop over multiple site folders
site_ids <- c("H5R0-01", "H5R0-02")  # Add more site IDs as needed , "H5R0-02" "H5R0-01"

for (site in site_ids) {
  cat("\n⏳ Processing site:", site, "\n")
  
  # Step 1: Set your paths (dynamic based on site)
  img_folder <- file.path("data/raw/data", site)
  roi_folder <- file.path("data/processed/roi_output", site)
  images_folder <-file.path("data/processed/roi_output",  site, "images")
  roi_file <- file.path(roi_folder, "imagesroi.data.Rdata")
  vi_folder <- file.path("data/processed/vi_output", site)
  date.code <- "yyyy-mm-dd"
  filter <- file.path("H5_filter", site, "bad")
  
  # Function to (re)create a folder: deletes existing and makes a fresh one
  recreate_folder <- function(folder_path) {
    if (dir.exists(folder_path)) {
      unlink(folder_path, recursive = TRUE)
    }
    dir.create(folder_path, recursive = TRUE)
  }
  
  # Recreate folders
  recreate_folder(roi_folder)
  recreate_folder(images_folder)
  recreate_folder(vi_folder)
  
  # Step 2: Get list of image files
  all_images <- list.files(img_folder, pattern = "\\.JPG$", full.names = TRUE)
  
  # Step 3: Filter out bad images
  bad_images <- list.files(filter, pattern = "\\.JPG$", full.names = FALSE)
  image_files <- all_images[!basename(all_images) %in% bad_images]
  
  cat("  Total images:", length(all_images), "\n")
  cat("  Filtered out:", length(bad_images), "\n")
  cat("  Remaining images:", length(image_files), "\n")
  
  DrawMULTIROI(
    path_img_ref = image_files[1],
    path_ROIs = images_folder,
    roi.names = c("RO1", "RO2"),
    nroi = 2
  )
  
  # Define source and destination paths
  ROI_from <- file.path(roi_file)
  ROI_to <- file.path(roi_folder, "roi.data.Rdata")
  file.rename(ROI_from, ROI_to)
  
  # Step 5: Extract Vegetation Indices (VIs)
  extractVIs(
    img.path = img_folder,
    roi.path = roi_folder,
    vi.path = vi_folder,
    date.code = date.code,
    log.file = "data/processed",
    ncores = 1,
    file.type = ".JPG"
  )
  
  # Move VI result
  VI_from <- file.path("data/processed/vi_output", paste0(site,  "VI.data.Rdata"))
  VI_to <- file.path(vi_folder, paste0(site,"VI.Rdata"))
  file.rename(VI_from, VI_to)
}

cat("\n✅ Done processing all sites!\n")

# Load the computed Vegetation Index (VI) data ----
load("data/processed/vi_output/")
VI.data
str(VI.data$RO1) 

# RO1 tibble 
RO1 <- VI.data$RO1 %>% 
  as.tibble() 

# RO2 tibble 
RO2 <- VI.data$RO2 %>% 
  as.tibble()

RO1 %>% 
  ggplot(mapping = aes(x = date, y = g.av)) + 
  geom_line() + 
  theme_minimal()

RO2 %>% 
  ggplot(mapping = aes(x = date, y = g.av)) + 
  geom_line() + 
  theme_minimal()

vi_2019_RO1 <- RO1 %>%
  filter(year(date) == 2019) %>%
  arrange(date) 

vi_2019_RO2 <- RO2 %>%
  filter(year(date) == 2019) %>%
  arrange(date) 

gcc_2019_RO1 <- zoo(vi_2019_RO1$g.av, order.by = vi_season$date)
gcc_2019_RO2 <- zoo(vi_2019_RO2$g.av, order.by = vi_season$date)

# Now try greenExplore!
explored_RO1 <- greenExplore(gcc_2019_RO1)
explored_RO2 <- greenExplore(gcc_2019_RO2)
plotExplore(explored_RO1)
plotExplore(explored_RO2)


