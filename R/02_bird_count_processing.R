# ===========================================================
# Script: 02_bird_count_processing.R
# Author: Anyll Markevich
# Date: 2026-07-31
# Description: Process raw bird point count data into usable formats
# ===========================================================

# 1. PACKAGES
library(here)
library(dplyr)

# 2. LOAD DATA
data <- read.csv(here::here("data", "raw", "bird_data", "bird_surveys.csv"))
codes <- read.csv(here::here("data", "raw", "bird_data", "bird_codes.csv"))
metadata <- read.csv(here::here("data", "raw", "bird_data", "bird_survey_metadata.csv"))

# Replace empty cells with NA values where this was not done automatically to make data handling easier and more idiomatic
codes$Alpha.Code[which(codes$Alpha.Code == "")] <- NA

# Make metadata survey names compatible with R column names for each survey by replacing white spaces with periods. This data column will later be removed when re-exporting the metadata as survey names will become redundant
metadata$Survey <- lapply(metadata$Survey, function(x) {
  gsub(" ", ".", x)
})

# Update column names in tables that will be re-exported to snake case for consistency
colnames(metadata) <- c("survey_name", "date", "start_time", "end_time", "low_temp", "high_temp", "wind_conditions", "weather_conditions", "year", "survey_order", "survey_number")

# 3. PROCESS DATA
# Reformat point count data from a table of plots and survey names to a list of individual observations (annotated with the observation date and other key metadata). This new format follows tidy data principles and is far more query-able with data analysis software tools
point_count_data <- data.frame(matrix(nrow = 0, ncol = 8))
for (this_plot in unique(data$Plot)) {
  plot_data <- subset(data, Plot == this_plot)
  for (this_survey in metadata$survey_name) {
    # Extract all bird observations for a give observation date on a given plot
    row_ids <- which(!is.na(plot_data[[this_survey]]))
    # Calculate the number of different items that need recorded (one for each kind of bird observed on a plot during a single survey)
    n_ids <- length(row_ids)
    # Combine all the data needed for a single bird observation entry in the data set
    observed_birds <- plot_data$Species[row_ids]
    observed_counts <- plot_data[[this_survey]][row_ids]
    observed_codes <- unlist(lapply(observed_birds, function(x) {
      codes$Alpha.Code[which(codes$Common.Name == x)]
    }))
    observation_plot <- plot_data$Plot[row_ids]
    observation_year <- rep(subset(metadata, survey_name == this_survey)$year, n_ids)
    observation_date <- rep(subset(metadata, survey_name == this_survey)$date, n_ids)
    observation_order <- rep(subset(metadata, survey_name == this_survey)$survey_order, n_ids)
    observation_number <- rep(subset(metadata, survey_name == this_survey)$survey_number, n_ids)
    # Save the data
    temp_data <- data.frame(year = observation_year, survey_order = observation_order, survey_number = observation_number, date = observation_date, plot = observation_plot, species = observed_birds, alpha_code = observed_codes, count = observed_counts)
    point_count_data <- rbind(point_count_data, temp_data)
  }
}
# Sensibly sort bird observations to aide further analysis and clean up the data
point_count_data <- point_count_data[order(point_count_data$year, point_count_data$survey_order, point_count_data$plot, point_count_data$alpha_code), ]

# Convert bird observation data into a summary of individual counts and species richness per plot
plot_data <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(plot_data) <- c("year", "plot", "bird_count", "bird_species")
for (this_year in unique(metadata$year)) {
  for (this_plot in unique(point_count_data$plot)) {
    # Extract data relevant to a specific plot in a specific year
    plot_point_count_data <- subset(point_count_data, year == this_year & plot == this_plot)
    # Calculate summary statistics: number of individuals observed and species richness
    bird_count <- sum(plot_point_count_data$count) / max(plot_point_count_data$survey_order)
    species_count <- plot_point_count_data |>
      subset(!is.na(alpha_code), select = alpha_code) |>
      unique() |>
      nrow() / max(plot_point_count_data$survey_order)
    # Save the data
    plot_data[nrow(plot_data) + 1, ] <- c(this_year, this_plot, bird_count, species_count)
  }
}

# 4. SAVE OUTPUTS
# Remove newly unnecessary metadata columns (thanks to new point count data format) and rearrange metadata columns to improve usability
metadata <- subset(metadata, select = -survey_name)
metadata <- metadata %>% dplyr::select(year, survey_order, survey_number, date, start_time, end_time, low_temp, high_temp, wind_conditions, weather_conditions)

# Save formatted complete bird point count data, now in an easily query-able format
write.csv(point_count_data, here::here("data", "processed", "bird_survey_data.csv"), row.names = FALSE)
# Save metadata on conditions and timing of each bird survey
write.csv(metadata, here::here("data", "processed", "bird_survey_metadata.csv"), row.names = FALSE)
# Save formatted summary of bird data for every plot (number of individuals and species richness)
write.csv(plot_data, here::here("data", "processed", "bird_plot_data.csv"), row.names = FALSE)
