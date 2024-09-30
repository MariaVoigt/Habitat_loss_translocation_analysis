#------------------------------------#
# Script to work on forest info 
# incoming from rescue locations
# STEPS
# 1. prep_and_extract_zonal_GPDS.py
# 2. this script
#
# ------------------------------------ #
# Script accompanying Sherman and Voigt et al.
# Purpose: To extract forest loss before orangutan capture 
# and assess whether the probability of capture is influenced by forest loss.
# ------------------------------------ #

# Clear workspace to ensure a clean environment
rm(list = ls())

# Load necessary libraries
library(tidyverse)   # For data manipulation and visualization
library(foreach)     # For parallelized operations (used in future versions)

# Set global options to improve display of numeric outputs
options(scipen = 100, digits = 4)

# ---------------------------- #
# Helper function for calculations
# ---------------------------- #

# Function to calculate annual percentage change in forest cover
# Note: Remove the '-' sign before 100 if you want to capture directional changes
calc.annual.change <- function(value_start, value_end, year_start, year_end, round_to = 2) {
  result <- (((value_end / value_start) ^ (1 / (year_end - year_start))) - 1) * -100
  return(round(result, round_to))
}

# Define the conversion factor for forest area
# We assume a resolution of 30m x 30m (in square meters), converting to hectares (ha)
res2ha <- 30 * 30 * 0.0001

# ---------------------------- #
# Data Loading and Preparation
# ---------------------------- #

# File paths for processed forest data (Sumatra example)
forest_stat_path_sumatra <- "/path/to/Sumatra_processed_forest_data.csv"

# Read in processed data for Sumatra (replace with correct file paths for other regions)
forest_summary <- read.csv(forest_stat_path_sumatra, stringsAsFactors = FALSE)

# Check summary of the dataset
summary(forest_summary)

# Standardize 'sex' column by converting "Male" and "Female" entries to lowercase for consistency
forest_summary$sex <- tolower(forest_summary$sex)

forest_summary <- forest_summary_sumatra %>% 
  mutate(forest_year = as.numeric(forest_year)) %>% 
  filter(capture_year <= 2015 & forest_year > 2005)  #only for these points do we have 5 years of forest pre and post rescue


# calculate the loss of forest overall
# to separate curves
# calculate loss in years prior
# consider following years
interval_i <- 5

for (i in 1:nrow(forest_summary)) {
  print(i)
  i_id <- forest_summary[i, "id"]
  i_year <- forest_summary[i, "forest_year"]
  if (i_year <= 2015){
  forest_summary[i, paste0("int_forest_before", interval_i)] <-
    forest_summary_sumatra[forest_summary_sumatra$id == i_id &
                     forest_summary_sumatra$forest_year == (i_year - interval_i), 
                     "intact_forest_ha"]
  forest_summary[i, paste0("int_forest_after", interval_i)] <-
    forest_summary_sumatra[forest_summary_sumatra$id == i_id &
                             forest_summary_sumatra$forest_year == (i_year + interval_i), 
                           "intact_forest_ha"]

  forest_summary[i, paste0("perc_int_forest_loss_before", interval_i)] <-
    100 - (round(forest_summary[i,
                                paste0("int_forest_before", 
                                       interval_i)] * 100/forest_summary[i, 
                                        "intact_forest_ha"] ))
  forest_summary[i, paste0("perc_int_forest_loss_after", interval_i)] <-
    100 - (round(forest_summary[i,
                                paste0("int_forest_after", 
                                       interval_i)] * 100/forest_summary[i, 
                                                                         "intact_forest_ha"] ))
  } else {print("nope")}
  }
  
forest_summary <- na.omit(forest_summary)

forest_summary_calc <- forest_summary %>% 
  filter(capture_year == forest_year)   #only for these points do we have 5 years of forest post rescue

head(forest_summary_calc )
#Calculate summary statistics
number_rows <- nrow(forest_summary_calc)
median_loss <- median(forest_summary_calc$perc_int_forest_loss_after5)
max_loss <- max(forest_summary_calc$perc_int_forest_loss_after5)
min_loss <- min(forest_summary_calc$perc_int_forest_loss_after5)
sd_loss <- sd(forest_summary_calc$perc_int_forest_loss_after5)

# Create a dataframe
summary_df <- data.frame(
  Statistic = c("N", "Median", "Max", "Min", "Standard Deviation"),
  Value = c(number_rows, median_loss, max_loss, min_loss, sd_loss)
)

summary_df

# Export to a CSV file
write.csv(summary_df, "/Users/maria/Desktop/Work/Wildlife_Impact/Rescue_location_forest_analysis/Sumatra/forest_loss_summary.csv", row.names = FALSE)

plot(table(forest_summary_calc$perc_int_forest_loss_after5))


