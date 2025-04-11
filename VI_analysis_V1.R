
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




### VI analysis

# List all site folders in the vi_output directory
VI_site_folders <- list.files("data/processed/vi_output/", full.names = TRUE)
VI_site_ids <- basename(VI_site_folders[file.info(VI_site_folders)$isdir])

# Specify the selected sites
selected_sites <- c("H5R0-01")


# Loop through the selected sites and load the .RData file for each
for (site in selected_sites) {
  # Construct the path to the RData file (assuming it's the only file in the folder)
  rdata_file <- file.path("data/processed/vi_output", site, paste0(site, "VI.Rdata"))
  
  # Check if the RData file exists before attempting to load
  if (file.exists(rdata_file)) {
    load(rdata_file)
    cat("Loaded file:", rdata_file, "\n")
  } else {
    cat("RData file for site", site, "not found.\n")
  }
}


# VI.data$RO1$NDVI <- (VI.data$RO1$g.av - VI.data$RO1$r.av) / (VI.data$RO1$g.av + VI.data$RO1$r.av)  ### NDVI??


# convert to tibble
VI.data <- VI.data$RO1 %>% 
  as_tibble()
VI.data

VI.data %>% 
  ggplot(mapping = aes(x = date, y = g.av)) + 
  geom_line()

table(lubridate::year(VI.data$date))


vi_season <- VI.data %>%
  filter(year(date) == 2019) %>%
  arrange(date) 


gcc_zoo <- zoo(vi_season$g.av, order.by = vi_season$date)

# Curve plots
explored <- greenExplore(gcc_zoo)
plotExplore(explored)

