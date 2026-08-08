# ===========================================================
# Script: 06_summarize_for_analysis.R
# Author: Anyll Markevich
# Date: 2026-08-08
# Description: Create simple summary tables from processed data that are ready for analysis
# ===========================================================

# 1. PACKAGES
library(here)
library(dplyr)

# 2. LOAD DATA
vegetation_data <- read.csv(here::here("data", "processed", "vegetation_plot_data.csv"))
bird_data <- read.csv(here::here("data", "processed", "bird_plot_data.csv"))
butterfly_data <- read.csv(here::here("data", "processed", "butterfly_plot_data.csv"))
mammal_data <- read.csv(here::here("data", "processed", "mammal_plot_data.csv"))
woody_cover_data <- read.csv(here::here("data", "processed", "woody_vegetation_plot_data.csv"))
geography_data <- read.csv(here::here("data", "processed", "plot_geography.csv"))
plot_data <- read.csv(here::here("data", "processed", "plot_info.csv"))

# 3. MAIN DATA SUMMARY
column_type <- data.frame(matrix(nrow = 0, ncol = 2))
# Metadata
main_summary <- select(mammal_data, year, plot)
column_type <- rbind(column_type, data.frame(column = colnames(main_summary), type = rep("metadata", ncol(main_summary))))
# Dependent variables (response)
main_summary <- cbind(main_summary, select(mammal_data, -year, -plot))
main_summary <- cbind(main_summary, select(bird_data, -year, -plot))
main_summary <- cbind(main_summary, select(butterfly_data, -year, -plot))
column_type <- main_summary %>%
  select(-column_type$column) %>%
  colnames() %>%
  data.frame(column = ., type = rep("response", length(.))) %>%
  rbind(column_type, .)
# Independent variables (predictors)
main_summary <- cbind(main_summary, select(vegetation_data, -year, -plot))
main_summary <- cbind(main_summary, select(woody_cover_data, -year, -plot))
main_summary$distance_to_highway <- rep(geography_data$distance_to_highway, 2)
main_summary$slope <- rep(geography_data$slope, 2)
column_type <- main_summary %>%
  select(-column_type$column) %>%
  colnames() %>%
  data.frame(column = ., type = rep("predictor", length(.))) %>%
  rbind(column_type, .)
# Plot categories (predictors)
main_summary$year_category <- 0
main_summary$year_category[which(main_summary$year == 2021)] <- 1
main_summary$will_burn <- vapply(main_summary$plot, FUN = function(x) {
  if (plot_data$calwood_fire_impact[which(plot_data$plot == x)] == "Burned") {
    return(1)
  } else {
    return(0)
  }
}, FUN.VALUE = c(0))
main_summary$burned <- 0
main_summary$burned[which(main_summary$year == 2021 & main_summary$will_burn == 1)] <- 1
column_type <- main_summary %>%
  select(-column_type$column) %>%
  colnames() %>%
  data.frame(column = ., type = rep("category", length(.))) %>%
  rbind(column_type, .)

# 4. VEGETATION DATA SUMMARY
# Metadata
veg_summary <- plot_data %>% select(plot)
# Vegetation variables
for (this_year in c(2020, 2021)) {
  temp_veg_vars <- vegetation_data %>%
    subset(year == this_year) %>%
    select(-year, -plot)
  colnames(temp_veg_vars) <- paste(colnames(temp_veg_vars), paste("_", this_year, sep = ""), sep = "")
  veg_summary <- cbind(veg_summary, temp_veg_vars)
}
# Plot characteristic variables
veg_summary <- cbind(veg_summary, select(geography_data, -plot))
veg_summary$burned <- plot_data$calwood_fire_impact %>% vapply(FUN = function(x) {
  if (x == "Burned") {
    return(1)
  } else {
    return(0)
  }
}, FUN.VALUE = c(0))

# 5. SAVE OUTPUTS
write.csv(main_summary, here::here("data", "summarized", "main_summary.csv"), row.names = FALSE)
write.csv(column_type, here::here("data", "summarized", "main_summary_columns.csv"), row.names = FALSE)
write.csv(veg_summary, here::here("data", "summarized", "vegetation_summary.csv"), row.names = FALSE)
