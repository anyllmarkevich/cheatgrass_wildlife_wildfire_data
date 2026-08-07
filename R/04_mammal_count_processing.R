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

# DEFINITIONS
# session_from_date_time("7/14", 1000, 1, dates_2021, times_2021)
session_from_date_time <- function(date, time, plot, date_table, time_table) {
  this_date <- split_date(date)
  good_ids <- c()
  for (i in 1:length(date_table[,1])) {
    start <- split_date(date_table$Start.Day[i])
    end <- split_date(date_table$End.Day[i])
    if (
      (this_date[1] > start[1] || (this_date[1] == start[1] && this_date[2] >= start[2])) &&
      (this_date[1] < end[1] || (this_date[1] == end[1] && this_date[2] <= end[2]))
    ) {
      good_ids <- c(good_ids, i)
    }
  }
  possible_sessions <- date_table[good_ids,]
  if (length(possible_sessions[,1]) > 1) {
    starting_session <- unname(unlist(subset(date_table, date == Start.Day, select = Session)))
    session_change_time <- times_2021[[paste("Start.Time.Session.", starting_session, sep = "")]][plot]
    if (time < session_change_time) {
      return(starting_session -1)
    } else {
      return(starting_session)
    }
  } else {
    return(possible_sessions$Session[1])
  }
}
split_date <- function(date) {
  return(as.integer(unlist(strsplit(date, "/"))))
}

# SOMETHING
# 2020
mammal_data <- data.frame(matrix(ncol = 7, nrow = 0))
for (time_code in dates_2020$Time.Code) {
  date <- dates_2020$Date[which(dates_2020$Time.Code == time_code)]
  session <- dates_2020$Session[which(dates_2020$Time.Code == time_code)]
  direction <- directions$Direction[which(directions$Session == session)]
  col_name <- paste("Time.", time_code, sep = "")
  
  row_ids <- which(data_2020[[col_name]] > 0)
  n_ids <- length(row_ids)
  
  observation_dates <- rep(date, n_ids)
  observation_sessions <- rep(session, n_ids)
  observation_directions <- rep(direction, n_ids)
  
  observation_plots <- data_2020$Plot[row_ids]
  observation_species <- data_2020$Species[row_ids]
  observation_counts <- subset(data_2020, select = col_name)[row_ids,1]
  
  temp_data <- data.frame(year = rep(2020, n_ids), plot = observation_plots, date = observation_dates, time = rep(NA, n_ids), session = observation_sessions, camera_direction = observation_directions, image = rep(NA, n_ids), species = observation_species, count = observation_counts)
  mammal_data <- rbind(mammal_data, temp_data)
}

# 2021
for (this_plot in data_2021$Plot) {
  plot_data <- subset(data_2021, Plot == this_plot)
  for (this_image in plot_data$Image) {
    image_data <- subset(plot_data, Image == this_image)
    animal_cols <- image_data %>% select(-Plot, -Image, -Date, - Time)
    this_date <- paste(split_date(substr(image_data$Date, 6, 10)), collapse = "/")
    this_time <- image_data$Time
    this_session <- session_from_date_time(this_date, this_time, this_plot, dates_2021, times_2021)
    this_direction <- directions$Direction[which(directions$Session == this_session)]
    for (this_species in colnames(animal_cols)) {
      if (all(!is.na(image_data[[this_species]]))) {
        this_count <- sum(image_data[[this_species]])
        temp_data <- c(2021, this_plot, this_date, this_time, this_session, this_direction, this_image, this_species, this_count)
        mammal_data <- rbind(mammal_data, temp_data)
      }
    }
  }
}






