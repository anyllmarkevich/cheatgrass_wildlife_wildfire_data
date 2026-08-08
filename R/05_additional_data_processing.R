# ===========================================================
# Script: 05_additional_data_processing.R
# Author: Anyll Markevich
# Date: 2026-08-08
# Description: Process and re-export additional data, including woody plant cover and plot characteristics
# ===========================================================

# 1. PACKAGES
library(here)

# 2. LOAD DATA
woody_cover_2020 <- read.csv(here::here("data", "raw", "additional_data", "woody_plant_cover_2020.csv"))
woody_cover_2021 <- read.csv(here::here("data", "raw", "additional_data", "woody_plant_cover_2021.csv"))
geography <- read.csv(here::here("data", "raw", "additional_data", "plot_geography.csv"))
info <- read.csv(here::here("data", "raw", "additional_data", "plot_info.csv"))

# Update column names in tables that will be re-exported to snake case for consistency
colnames(geography) <- c("plot", "distance_to_highway", "slope")
colnames(info) <- c("plot", "latitude", "longitude", "property", "access_property", "cheatgrass_level_2020", "cheatgrass_level_2021", "calwood_fire_impact")

# 3. PROCESS DATA
# Combine woody cover data from 2020 and 2021
# NOTE: Living woody cover will be identical to total woody plant cover for 2020 measurements as this value was not measured separately in 2020
woody_cover_data <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(woody_cover_data) <- c("year", "plot", "total_woody_cover", "living_woody_cover")
# Process 2020 data
for (this_plot in 1:8) {
  total_woody_cover <- woody_cover_2020$Woody.Cover[which(woody_cover_2020$Plot == this_plot)]
  woody_cover_data[nrow(woody_cover_data) + 1, ] <- c(2020, this_plot, total_woody_cover, total_woody_cover)
}
# Process 2021 data
for (this_plot in 1:8) {
  total_woody_cover <- woody_cover_2020$Woody.Cover[which(woody_cover_2020$Plot == this_plot)]
  living_woody_cover <- woody_cover_2021$Living.Woody.Cover[which(woody_cover_2021$Plot == this_plot)]
  woody_cover_data[nrow(woody_cover_data) + 1, ] <- c(2021, this_plot, total_woody_cover, living_woody_cover)
}

# 5. SAVE OUTPUTS
write.csv(woody_cover_data, here::here("data", "processed", "woody_vegetation_plot_data.csv"), row.names = FALSE)
write.csv(geography, here::here("data", "processed", "plot_geography.csv"), row.names = FALSE)
write.csv(info, here::here("data", "processed", "plot_info.csv"), row.names = FALSE)
