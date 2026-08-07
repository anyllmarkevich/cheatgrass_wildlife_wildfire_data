# ===========================================================
# Script: 04_mammal_count_processing.R
# Author: Anyll Markevich
# Date: 2026-08-06
# Description: Process raw mammal count data into usable formats
# ===========================================================

# 1. PACKAGES
library(here)
library(dplyr)

# 2. LOAD DATA
data_2020 <- read.csv(here::here("data", "raw", "mammal_data", "mammal_survey_2020.csv"))
data_2021 <- read.csv(here::here("data", "raw", "mammal_data", "mammal_survey_2021.csv"))
dates_2020 <- read.csv(here::here("data", "raw", "mammal_data", "mammal_dates_2020.csv"))
dates_2021 <- read.csv(here::here("data", "raw", "mammal_data", "mammal_dates_2021.csv"))
times_2021 <- read.csv(here::here("data", "raw", "mammal_data", "mammal_times_2021.csv"))
directions <- read.csv(here::here("data", "raw", "mammal_data", "mammal_camera_directions.csv"))

# 3. DEFINITIONS
# Convert a date formatted as a "MM/DD" string into a vector with two separate numerical values
split_date <- function(date) {
  return(as.integer(unlist(strsplit(date, "/"))))
}

# Find the camera trap session based on the date and time of an image (and reference data)
session_from_date_time <- function(date, time, plot, date_table, time_table) {
  this_date <- split_date(date)
  good_ids <- c()
  # Check each session to see if the given date may be part of it
  for (i in 1:length(date_table[, 1])) {
    start <- split_date(date_table$Start.Day[i])
    end <- split_date(date_table$End.Day[i])
    # Check if the given date is contained within the session
    if (
      (this_date[1] > start[1] || (this_date[1] == start[1] && this_date[2] >= start[2])) &&
        (this_date[1] < end[1] || (this_date[1] == end[1] && this_date[2] <= end[2]))
    ) {
      good_ids <- c(good_ids, i)
    }
  }
  # Keep only possible sessions
  possible_sessions <- date_table[good_ids, ]
  # Return the correct session
  if (length(possible_sessions[, 1]) > 1) { # If multiple sessions are possible based on the data, return the correct session based on the time
    starting_session <- unname(unlist(subset(date_table, date == Start.Day, select = Session)))
    session_change_time <- times_2021[[paste("Start.Time.Session.", starting_session, sep = "")]][plot]
    if (time < session_change_time) { # If the photo was taken before the session change, return the previous session
      return(starting_session - 1)
    } else { # If the photo was taken after the session change, return the next session
      return(starting_session)
    }
  } else { # If the photo could only come from one session, return that session
    return(possible_sessions$Session[1])
  }
}

# 4. PROCESS DATA
# Reformat 2020 camera trap data from a table of species and observation dates to a list of individual observations. This new format follows tidy data principles and is far more query-able with data analysis software tools
# NOTE: Due to inferior data collecting methodology in 2020, the specific photos in which animals were observed, along with the exact time these photos were taken, is not included in this data. Procuring this data would require a labor-intensive repeat of manual data collection from photos
trailcam_photo_data <- data.frame(matrix(ncol = 7, nrow = 0))
# For each time period (usually a day) listed in the data (each is a column), extract the data and reformat it
for (time_code in dates_2020$Time.Code) {
  # Get high-level information on the time period
  date <- dates_2020$Date[which(dates_2020$Time.Code == time_code)]
  session <- dates_2020$Session[which(dates_2020$Time.Code == time_code)]
  direction <- directions$Direction[which(directions$Session == session)]
  col_name <- paste("Time.", time_code, sep = "")
  # Extract the data relevant to that time period
  row_ids <- which(data_2020[[col_name]] > 0)
  n_ids <- length(row_ids)
  # Combine all the data needed for a single butterfly observation entry in the data set
  observation_dates <- rep(date, n_ids)
  observation_sessions <- rep(session, n_ids)
  observation_directions <- rep(direction, n_ids)
  observation_plots <- data_2020$Plot[row_ids]
  observation_species <- data_2020$Species[row_ids]
  observation_counts <- subset(data_2020, select = col_name)[row_ids, 1]
  # Update species names to be consistent with 2021 data
  observation_species <- gsub("Misc\\.", "Unidentified", observation_species)
  observation_species[which(observation_species == "Ungulate")] <- "Unidentified Ungulate"
  # Save the data
  temp_data <- data.frame(year = rep(2020, n_ids), plot = observation_plots, date = observation_dates, time = rep(NA, n_ids), session = observation_sessions, camera_direction = observation_directions, image = rep(NA, n_ids), species = observation_species, count = observation_counts)
  trailcam_photo_data <- rbind(trailcam_photo_data, temp_data)
}

# Reformat 2021 camera trap data from a list of a subset of photographs that might contain animals to a list of individual animal observations. This new format follows tidy data principles and is far more query-able with data analysis software tools
for (this_plot in unique(data_2021$Plot)) {
  plot_data <- subset(data_2021, Plot == this_plot)
  for (this_image in unique(plot_data$Image)) {
    # Extract the data relevant to a specific image on a specific plot
    image_data <- subset(plot_data, Image == this_image)
    this_date <- paste(split_date(substr(image_data$Date, 6, 10)), collapse = "/")
    this_time <- image_data$Time
    this_session <- session_from_date_time(this_date, this_time, this_plot, dates_2021, times_2021)
    this_direction <- directions$Direction[which(directions$Session == this_session)]
    # Isolate animal data
    animal_cols <- image_data %>% select(-Plot, -Image, -Date, -Time)
    # If the photo contained animals, record an observation
    if (!all(is.na(animal_cols[1, ]))) {
      # Record the observed species and their counts
      species <- colnames(animal_cols)[which(!is.na(animal_cols[1, ]))]
      counts <- animal_cols[1, which(!is.na(animal_cols[1, ]))] |> unname()
      for (i in 1:length(species)) {
        # Convert column names to consistent taxa names
        this_species <- gsub("\\.", " ", species[i])
        # Save the data
        temp_data <- c(2021, this_plot, this_date, this_time, this_session, this_direction, this_image, this_species, counts[i])
        trailcam_photo_data <- rbind(trailcam_photo_data, temp_data)
      }
    }
  }
}

# Convert mammal observation data into a summary of individual counts and species richness per plot
plot_data <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(plot_data) <- c("year", "plot", "mammal_count", "mammal_species")
for (this_year in c(2020, 2021)) {
  for (this_plot in 1:8) {
    # Extract data relevant to a specific plot in a specific year
    plot_trailcam_photo_data <- subset(trailcam_photo_data, year == this_year & plot == this_plot)
    # Calculate summary statistics: number of individuals observed and species richness
    mammal_count <- sum(as.integer(plot_trailcam_photo_data$count))
    species_count <- plot_trailcam_photo_data |>
      subset(!(species %in% c("Unidentified Deer", "Unidentified Mammal", "Unidentified Ungulate")), select = species) |>
      unique() |>
      nrow()
    # Save the data
    plot_data[nrow(plot_data) + 1, ] <- c(this_year, this_plot, mammal_count, species_count)
  }
}

# 5. SAVE OUTPUTS
write.csv(trailcam_photo_data, here::here("data", "processed", "mammal_survey_data.csv"), row.names = FALSE)
