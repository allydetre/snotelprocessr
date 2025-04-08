### Script for extracting pulse dates from processed SNOTEL data ###

# *Important to note: this script should be run after running process_snotel_data.R

# This script will loop through the processed SNOTEL data CSVs and extract the soil moisture pulse date/DOYs for each year

################################################################################

library(tidyverse)
library(snotelprocessr)

# Define folder path
folder_path <- "path/to/your/processed/SNOTEL/data/folder"
  
# Initialize empty df to hold results
all_pulses <- data.frame()

# Make a list of all csvs in folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# For loop to get pulse dates from the stations for each year:
for (file_path in csv_files) {
  # Fill in station name
  station_name <- str_extract(basename(file_path), "^[^_]+_[^_]+")
  
  # Read CSV file into a df with explicit column types
  df <- read_csv(file_path, col_types = cols(
    year = col_double(),
    doy = col_double(),
    swe = col_double(),
    soilm_p2 = col_double(),
    # Add any other columns that need to be numeric here
    .default = col_character()  # Default to character for other columns
  ),
  show_col_types = FALSE)
  
  # Check if data frame is empty
  if (nrow(df) > 0) {
    # Apply pulse function
    ### ***CHANGE PARAMS AS NEEDED*** ###
    pulse_results <- snotel_detect_smpulse(
      df = df, 
      soil_moisture_col = "soilm_p2", 
      doy_col = "doy", 
      year_col = "year", 
      swe_col = "swe", 
      min_swe_window_before = 70, 
      max_swe_window_after = 30, 
      variability_multiplier = 1.25, 
      derivative_length = 2
    )
    
    # Add station_name column
    pulse_results <- pulse_results %>%
      mutate(station_name = station_name)
    
    # Append results to all_pulses
    all_pulses <- bind_rows(all_pulses, pulse_results)
  } else {
    message(paste("No data in file:", basename(file_path)))
  }
}

# Write the final all_pulses data frame to a CSV file
write.csv(all_pulses, "path/to/your/output/csv", row.names = FALSE)
