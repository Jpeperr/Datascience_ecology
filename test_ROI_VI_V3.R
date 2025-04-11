### PACKAGES
auto_install_package <- function(package_names_list) {
  for (package_name in package_names_list) {
    if (!(package_name %in% rownames(installed.packages()))) {
      install.packages(package_name, dependencies = TRUE)
    }
    library(package_name, character.only = TRUE)
  }
}

auto_install_package(c("tidyverse", "phenofit", "bfast", "phenocamr", "phenopix",
                       "imager", "tools", "zoo"))

### SITE CONFIGURATION
site_ids <- c("H5R0-01")
filter_groups <- list(
  H5 = "H5_filter"
)

recreate_folder <- function(folder_path) {
  if (dir.exists(folder_path)) unlink(folder_path, recursive = TRUE)
  dir.create(folder_path, recursive = TRUE)
}

# Store path objects for each site
site_paths <- list()

### LOOP 1: ROI Drawing
for (site in site_ids) {
  cat("Drawing ROI for site:", site, "\n")
  
  site_prefix <- substr(site, 1, 2)
  filter_root <- filter_groups[[site_prefix]]
  if (is.null(filter_root)) stop(paste("No filter folder defined for site prefix:", site_prefix))
  
  # Define paths
  img_folder <- file.path("data/raw/data", site)
  roi_folder <- file.path("data/processed/roi_output", site)
  images_folder <- file.path(roi_folder, "images")
  roi_file <- file.path(roi_folder, "imagesroi.data.Rdata")
  roi_file_final <- file.path(roi_folder, "roi.data.Rdata")
  roi_input_folder <- file.path("data/input", site)
  filter_folder <- file.path(filter_root, site, "bad")
  
  # Save for VI step
  site_paths[[site]] <- list(
    site = site,
    img_folder = img_folder,
    roi_folder = roi_folder,
    images_folder = images_folder,
    roi_file_final = roi_file_final,
    roi_input_folder = roi_input_folder,
    vi_folder = file.path("data/processed/vi_output", site),
    vi_file_temp = file.path("data/processed/vi_output", paste0(site, "VI.data.Rdata")),
    vi_file_final = file.path("data/processed/vi_output", site, paste0(site, "VI.Rdata")),
    filter_folder = filter_folder
  )
  
  recreate_folder(roi_folder)
  recreate_folder(images_folder)
  
  # Get image files
  all_images <- list.files(img_folder, pattern = "\\.JPG$", full.names = TRUE)
  bad_images <- list.files(filter_folder, pattern = "\\.JPG$", full.names = FALSE)
  image_files <- all_images[!basename(all_images) %in% bad_images]
  
  cat(" Total:", length(all_images), " | Filtered:", length(bad_images), " | Used:", length(image_files), "\n")
  
  use_input_roi <- readline(prompt = paste0("Use predefined ROI for site ", site, "? (y/n): "))
  
  if (tolower(use_input_roi) == "n") {
    DrawMULTIROI(
      path_img_ref = image_files[1],
      path_ROIs = images_folder,
      roi.names = "RO1",
      nroi = 1
    )
    file.rename(roi_file, roi_file_final)
  } else if (tolower(use_input_roi) == "y") {
    site_paths[[site]]$roi_folder <- roi_input_folder
    roi_file_final <- file.path(roi_input_folder, "roi.data.Rdata")
    if (!file.exists(roi_file_final)) stop("Predefined ROI file missing for site: ", site)
    cat("Using predefined ROI\n")
  } else {
    stop("Invalid input. Please type 'y' or 'n'.")
  }
}

### LOOP 2: VI Extraction
cat("Starting VI extraction...\n")

for (site in names(site_paths)) {
  paths <- site_paths[[site]]
  cat("Extracting VIs for site:", site, "\n")
  
  recreate_folder(paths$vi_folder)
  
  extractVIs(
    img.path = paths$img_folder,
    roi.path = paths$roi_folder,
    vi.path = paths$vi_folder,
    date.code = "yyyy-mm-dd",
    log.file = "data/processed",
    ncores = 1,
    file.type = ".JPG"
  )
  
  file.rename(paths$vi_file_temp, paths$vi_file_final)
  cat("VI file saved:", paths$vi_file_final, "\n")
}

cat("All sites processed successfully!\n")



