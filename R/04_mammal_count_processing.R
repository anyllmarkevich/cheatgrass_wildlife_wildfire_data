# ===========================================================
# Script: 04_mammal_count_processing.R
# Author: Anyll Markevich
# Date: 2026-08-06
# Description: Process raw mammal count data into usable formats
# ===========================================================

# 1. PACKAGES
library(here)

# 2. LOAD DATA
data_2020 <- read.csv(here::here("data", "raw", "mammal_data", "mammal_survey_2020.csv"))
data_2021 <- read.csv(here::here("data", "raw", "mammal_data", "mammal_survey_2021.csv"))
dates <- read.csv(here::here("data", "raw", "mammal_data", "mammal_dates.csv"))
directions <- read.csv(here::here("data", "raw", "mammal_data", "mammal_camera_directions.csv"))