# ===========================================================
# Script: 06_summarize_for_analysis.R
# Author: Anyll Markevich
# Date: 2026-08-08
# Description: Create simple summary tables from processed data that are ready for analysis
# ===========================================================

# 1. PACKAGES
library(here)

# 2. LOAD DATA
vegetation_data <- read.csv(here::here("data", "processed", "vegetation_plot_data.csv"))
bird_data <- read.csv(here::here("data", "processed", "bird_plot_data.csv"))
butterfly_data <- read.csv(here::here("data", "processed", "butterfly_plot_data.csv"))
mammal_data <- read.csv(here::here("data", "processed", "mammal_plot_data.csv"))