# ===========================================================
# Script: 01_vegetation_survey_processing.R
# Author: Anyll Markevich
# Date: 2026-07-27
# Description: Process raw vegetation survey data into usable formats
# ===========================================================

# 1. PACKAGES
library(here)

# 2. LOAD DATA
data_2020 <- read.csv(here::here("data", "raw", "vegetation_data", "vegetation_survey_2020.csv"))
data_2021 <- read.csv(here::here("data", "raw", "vegetation_data", "vegetation_survey_2021.csv"))

# Update column names to snake case for consistency when re-exporting formatted tables
colnames(data_2020) <- c("plot", "direction", "distance", "column", "row", "content_id")
colnames(data_2021) <- c("plot", "direction", "distance", "column", "row", "content_id")

# 3. DEFINITIONS
# Define all possible vegetation categories (order matters)
veg_categories <- c("cheatgrass_plant", "cheatgrass_litter", "bare_ground", "other_litter", "other_plant")

# Convert a table of how many times each ID occurs into a vector that contains the count of all IDs, matching the columns of a summary table
table_to_row <- function(table) {
  veg_categories <- c("cheatgrass_plant", "cheatgrass_litter", "bare_ground", "other_litter", "other_plant")
  row <- rep(0, 5)
  for (content in names(table)) {
    content_num <- which(veg_categories == content)
    row[as.integer(content_num)] <- table[[content]]
  }
  as.vector(row)
}

# 4. COMBINE DATA
# Prepare to merge 2020 and 2021 data by adding a year column to each data set
data_2020 <- cbind(year = rep(2020, nrow(data_2020)), data_2020)
data_2021 <- cbind(year = rep(2021, nrow(data_2021)), data_2021)

# Combine the 2020 and 2021 into a single sorted data set to make further analysis easier
point_data <- rbind(data_2020, data_2021)
point_data <- point_data[order(point_data$year, point_data$plot, point_data$direction, point_data$distance, point_data$column, point_data$row), ]

# 5. PROCESS DATA
# Convert numerical vegetation categories into readable categories
point_data$content_id <- sapply(point_data$content_id, function(id, veg_type = veg_categories) veg_type[id])
colnames(point_data)[which(colnames(point_data) == "content_id")] <- "content"

# Summarize the contents of each sampling frame by counting the number of each content ID within a frame
sample_data <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(sample_data) <- c("year", "plot", "direction", "distance", veg_categories)
for (this_year in unique(point_data$year)) {
  for (this_plot in unique(point_data$plot)) {
    for (this_direction in unique(point_data$direction)) {
      for (this_distance in unique(point_data$distance)) {
        sample <- point_data |>
          subset(year == this_year & plot == this_plot & direction == this_direction & distance == this_distance, select = "content") |>
          table() |>
          table_to_row()
        sample_data[nrow(sample_data) + 1, ] <- c(this_year, this_plot, this_direction, this_distance, sample)
      }
    }
  }
}

# Summarize the contents of each plot by finding the proportion of each vegetation type on each plot
plot_data <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(plot_data) <- c("year", "plot", veg_categories)
for (this_year in unique(sample_data$year)) {
  for (this_plot in unique(sample_data$plot)) {
    proportions <- subset(sample_data, plot == this_plot & year == this_year, select = veg_categories) |>
      colSums() |>
      proportions()
    plot_data[nrow(plot_data) + 1, ] <- c(this_year, this_plot, proportions)
  }
}

# 6. SAVE OUTPUTS
# Save formatted vegetation data for every sampling point (a single pin within a sampling quadrant)
write.csv(point_data, here::here("data", "processed", "vegetation_point_data.csv"), row.names = FALSE)
# Save formatted summary of vegetation data for every sampling frame (a whole sampling quadrant)
write.csv(sample_data, here::here("data", "processed", "vegetation_pampling_frame_data.csv"), row.names = FALSE)
# Save formatted summary of vegetation data for every plot
write.csv(plot_data, here::here("data", "processed", "vegetation_plot_data.csv"), row.names = FALSE)
