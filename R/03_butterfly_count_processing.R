# ===========================================================
# Script: 03_butterfly_count_processing.R
# Author: Anyll Markevich
# Date: 2026-08-02
# Description: Process raw butterfly transect count data into usable formats
# ===========================================================

# 1. PACKAGES
library(here)
library(dplyr)

# 2. LOAD DATA
data <- read.csv(here::here("data", "raw", "butterfly_data", "butterfly_surveys.csv"))
codes <- read.csv(here::here("data", "raw", "butterfly_data", "butterfly_codes.csv"))
metadata <- read.csv(here::here("data", "raw", "butterfly_data", "butterfly_survey_metadata.csv"))
dates <- read.csv(here::here("data", "raw", "butterfly_data", "butterfly_survey_dates.csv"))
times <- read.csv(here::here("data", "raw", "butterfly_data", "butterfly_survey_times.csv"))

# Replace empty cells with NA values where this was not done automatically to make data handling easier and more idiomatic
codes$Higher.Taxon[which(codes$Higher.Taxon == "")] <- NA

# Make metadata survey names compatible with R column names for each survey by replacing white spaces with periods. This data column will later be removed when re-exporting the metadata as survey names will become redundant
metadata$Survey <- lapply(metadata$Survey, function(x) {
  gsub(" ", ".", x)
})

# Update column names in tables that will be re-exported to snake case for consistency
colnames(metadata) <- c("survey_name", "start_time", "end_time", "low_temp", "high_temp", "wind_speed", "wind_direction", "low_cloud_cover", "high_cloud_cover", "year", "survey_order", "survey_number", "multiple_dates")

# 3. DEFINITIONS
# Count the number of species observed within a subset of rows in formatted butterfly data, avoiding potential double-counting when butterfly species identifications where inexact (eg. "Sulphur species" shouldn't be counted if a "Cloudy sulphur" was observed)
count_species <- function(butterfly_data_rows) {
  # Extract info about every unique species entry
  species_list <- unique(butterfly_data_rows$species)
  exact_id <- unlist(lapply(species_list, function(x) {
    butterfly_data_rows$exact_id[which(butterfly_data_rows$species == x)[1]]
  }))
  informative_id <- unlist(lapply(species_list, function(x) {
    butterfly_data_rows$informative_id[which(butterfly_data_rows$species == x)[1]]
  }))
  higher_taxon <- unlist(lapply(species_list, function(x) {
    butterfly_data_rows$group_name[which(butterfly_data_rows$species == x)[1]]
  }))
  unique_species <- 0
  # Check whether each unique species entry should be counted as a separate species, and update the total accordingly
  for (i in 1:length(species_list)) {
    if (informative_id[i] == 0 && length(species_list) == 1) { # If there is only one observation and the identification is uninformative (eg. "Butterfly species"), then at least one species was observed
      unique_species <- 1
    } else if (exact_id[i] == 0 && !(species_list[i] %in% higher_taxon) && informative_id[i] == 1) { # If the observation has an inexact but informative species identification, only count that species if no species within that taxon were observed
      unique_species <- unique_species + 1
    } else if (exact_id[i] == 1) { # If the observation has an exact species identification, count that species
      unique_species <- unique_species + 1
    }
  }
  return(unique_species)
}

# 4. PROCESS DATA
# Reformat transect count data from a table of plots and survey names to a list of individual observations (annotated with the observation date and other key metadata). This new format follows tidy data principles and is far more query-able with data analysis software tools
transect_count_data <- data.frame(matrix(nrow = 0, ncol = 11))
for (this_plot in unique(data$Plot)) {
  plot_data <- subset(data, Plot == this_plot)
  for (this_survey in metadata$survey_name) {
    # Extract all butterfly observations for a give observation date on a given plot
    row_ids <- which(!is.na(plot_data[[this_survey]]))
    # Calculate the number of different items that need recorded (one for each kind of butterfly observed on a plot during a single survey)
    n_ids <- length(row_ids)
    # Combine all the data needed for a single butterfly observation entry in the data set
    observed_butterflies <- plot_data$Species[row_ids]
    observed_counts <- plot_data[[this_survey]][row_ids]
    observation_plot <- plot_data$Plot[row_ids]
    observation_year <- rep(subset(metadata, survey_name == this_survey)$year, n_ids)
    observation_date <- rep(dates[which(dates$Plot == this_plot), which(colnames(dates) == this_survey)], n_ids)
    observation_time <- rep(times[which(times$Plot == this_plot), which(colnames(times) == this_survey)], n_ids)
    observation_order <- rep(subset(metadata, survey_name == this_survey)$survey_order, n_ids)
    observation_number <- rep(subset(metadata, survey_name == this_survey)$survey_number, n_ids)
    # This data is useful to avoid potential double-counting of species when inexact species IDs are used
    observation_exact <- unlist(lapply(observed_butterflies, function(x) {
      codes$Exact.ID[which(codes$Common.Name == x)]
    }))
    observation_informative <- unlist(lapply(observed_butterflies, function(x) {
      codes$Informative.ID[which(codes$Common.Name == x)]
    }))
    observation_taxon <- unlist(lapply(observed_butterflies, function(x) {
      codes$Higher.Taxon[which(codes$Common.Name == x)]
    }))
    # Save the data
    temp_data <- data.frame(year = observation_year, survey_order = observation_order, survey_number = observation_number, date = observation_date, time = observation_time, plot = observation_plot, species = observed_butterflies, count = observed_counts, exact_id = observation_exact, informative_id = observation_informative, group_name = observation_taxon)
    transect_count_data <- rbind(transect_count_data, temp_data)
  }
}
# Sensibly sort butterfly observations to aide further analysis and clean up the data
transect_count_data <- transect_count_data[order(transect_count_data$year, transect_count_data$survey_order, transect_count_data$plot, transect_count_data$species), ]

# Convert butterfly observation data into a summary of individual counts and species richness per plot
# NOTE: For consistency, only 4 of the 6 surveys in 2020 are included, as there were only 4 surveys in 2021
plot_data <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(plot_data) <- c("year", "plot", "butterfly_count", "butterfly_species")
for (this_year in unique(metadata$year)) {
  for (this_plot in unique(transect_count_data$plot)) {
    # Extract data relevant to a specific plot in a specific year
    plot_transect_count_data <- subset(transect_count_data, year == this_year & plot == this_plot & survey_order <= 4)
    # Calculate summary statistics: number of individuals observed and species richness. Un-comment the end of the next two lines of codes to express these values per butterfly count, as opposed to total individual and species counts.
    butterfly_count <- sum(plot_transect_count_data$count) #/ max(plot_transect_count_data$survey_order)
    species_count <- count_species(plot_transect_count_data) #/ max(plot_transect_count_data$survey_order)
    # Save the data
    plot_data[nrow(plot_data) + 1, ] <- c(this_year, this_plot, butterfly_count, species_count)
  }
}

# 5. SAVE OUTPUTS
# Remove newly unnecessary metadata columns (thanks to new transect count data format) and rearrange metadata columns to improve usability
metadata <- subset(metadata, select = -survey_name)
metadata <- metadata %>% dplyr::select(year, survey_order, survey_number, start_time, end_time, low_temp, high_temp, wind_speed, wind_direction, low_cloud_cover, high_cloud_cover, multiple_dates)

# Save formatted complete butterfly transect count data, now in an easily query-able format
write.csv(transect_count_data, here::here("data", "processed", "butterfly_survey_data.csv"), row.names = FALSE)
# Save metadata on conditions and timing of each butterfly survey
write.csv(metadata, here::here("data", "processed", "butterfly_survey_metadata.csv"), row.names = FALSE)
# Save formatted summary of butterfly data for every plot (number of individuals and species richness)
write.csv(plot_data, here::here("data", "processed", "butterfly_plot_data.csv"), row.names = FALSE)
