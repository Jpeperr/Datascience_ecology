
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
                       "imager", "tools"))

### NOTE
#' script assumes a main data folder exits in the same location as the script.
#' data contains a "raw" folder (which contains the raw image data) and a processed"
#' folder that will be filled with the processed images/data.

# Step 1: Set your paths
img_folder <- "data/raw/data/H5R0-01"  # <-- replace with your image folder path
roi_folder <- "data/processed/roi_output"                  # folder to save ROI image previews
roi_file <- "data/processed/roi_output/roi.data.Rdata"                # path to saved ROI definition
vi_folder <- "data/processed/vi_output"             # optional: where to save VI output
date.code <- "yyyy-mm-dd-HHhMMmSS"


# Function to (re)create a folder: deletes existing and makes a fresh one
recreate_folder <- function(folder_path) {
  if (dir.exists(folder_path)) {
    unlink(folder_path, recursive = TRUE)  # delete the folder and its contents
  }
  dir.create(folder_path, recursive = TRUE)  # create new folder
}

# Recreate folders
recreate_folder(roi_folder)
recreate_folder(vi_folder)



# Step 2: Get list of image files and filter out bad photos
bad_folder <- "H5_filter/H5R0-01/bad"

# List all file names
image_files <- list.files(img_folder, pattern = "\\.JPG$", full.names = TRUE)

bad_files <- list.files(bad_folder, pattern = "\\.JPG$", full.names = FALSE)

# Filter out the bad ones (keep full path for image_files)
filtered_files <- image_files[!basename(image_files) %in% bad_files]

DrawMULTIROI(
    path_img_ref = filtered_files[1],  # Path to the first image
    path_ROIs = roi_folder,            # Directory to store ROI and coordinates
    roi.names = "RO1",
    nroi = 1,                       # Number of ROIs to draw
  )

# Define source and destination paths
ROI_from <- "data/processed/roi_outputroi.data.Rdata"
ROI_to <- file.path("data/processed/roi_output", "roi.data.RData")

# Move the file
file.rename(ROI_from, ROI_to)

# Step 5: Extract Vegetation Indices (VIs) from the images based on the ROI
extractVIs(
  img.path = filtered_files,       # Path to the image folder
  roi.path = roi_folder,          # Path to the saved ROI .RData file
  vi.path = vi_folder,         # Path to save the VIs
  date.code = date.code,  
  log.file = "data/processed",
  file.type='.JPG' # <- !!!
)

# Define source and destination paths
VI_from <- "data/processed/vi_outputVI.data.Rdata"
VI_to <- file.path("data/processed/vi_output", "vi_outputVI.data.Rdata")

# Move the file
file.rename(VI_from, VI_to)


load("data/processed/vi_output/vi_outputVI.data.Rdata")







