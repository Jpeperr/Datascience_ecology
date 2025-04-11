
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
img_folder <- "data/raw/data/H1R0-02/"  # <-- replace with your image folder path
roi_folder <- "data/processed/roi_output"                  # folder to save ROI image previews
roi_file <- "data/processed/roi_output/roi.data.Rdata"                # path to saved ROI definition
vi_folder <- "data/processed/vi_output"             # optional: where to save VI output
date.code <- "yyyy-mm-dd-HH-MM-SS"


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



# Step 2: Get list of image files
image_files <- list.files(img_folder, pattern = "\\.JPG$", full.names = TRUE)


DrawMULTIROI(
  path_img_ref = image_files[1],  # Path to the first image
  path_ROIs = roi_folder,            # Directory to store ROI and coordinates
  roi.names = c("RO1", "RO2"),
  nroi = 2,                       # Number of ROIs to draw
)


# Define source and destination paths
ROI_from <- "data/processed/roi_outputroi.data.Rdata"
ROI_to <- file.path("data/processed/roi_output", "roi.data.RData")

# load R data 
load("data/processed/roi_outputroi.data.Rdata") 
roi.data

library(raster)
# Plot RO1 mask
plot(roi.data$RO1$mask, main = "RO1 Mask")

# Plot RO2 mask
plot(roi.data$RO2$mask, main = "RO2 Mask")


# Move the file
file.rename(ROI_from, ROI_to)

img_proc_folder <- "data/processed/img_proc_folder"  # where to store renamed files

# Create the processed folder if it doesn't exist
recreate_folder(img_proc_folder)


# Step 4: Preprocess filenames and rename them
for (filename in image_files) {
  
  # Extract the filename from the path
  filename_clean <- basename(filename)
  
  # Preprocess the filename to replace 'h', 'm', and 's' with appropriate symbols or remove them
  filename_clean <- gsub("h", "-", filename_clean)  # replace 'h' with '-'
  filename_clean <- gsub("m", "-", filename_clean)  # replace 'm' with '-'
  filename_clean <- gsub("00", "00", filename_clean)  # keep '00' if needed
  
  # Get the full path of the new filename
  new_filename <- file.path(img_proc_folder, filename_clean)
  
  # Copy the original file to the new location with the new name
  file.copy(from = filename, to = new_filename, overwrite = TRUE)
  
  
  # Print renaming info (optional, to track changes)
  print(paste("Renamed:", filename, "to", new_filename))
}



# Step 5: Extract Vegetation Indices (VIs) from the images based on the ROI
extractVIs(
  img.path = img_proc_folder,       # Path to the image folder
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



# Load the computed Vegetation Index (VI) data ----
load("data/processed/vi_output/vi_outputVI.data.Rdata")
VI.data
str(VI.data$RO1) # this is a list with 1 element (roi1) which is a data.frame

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


