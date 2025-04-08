### SNOTEL station data processing script ###

# *Important to note: this script should be ran after raw data download using snotelprocessr package

# This script will loop through the raw SNOTEL data and export new, processed CSVs

################################################################################

library(tidyverse)
library(snotelprocessr)

# define input and output folders

# input: where raw data is located
input_folder <- "your/input/path"

# output: where processed data will go
output_folder <- "your/output/path"
  
  
# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

file_list <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

for (file_path in file_list) {
  # Read CSV
  df <- read.csv(file_path, fileEncoding = "UTF-8")
  
  # Apply processing functions (from snotelprocessr)
  df <- df %>%
    snotel_rename_col_daily()
  
  # Remove years with only NA values in soilm_p2 column
  ### comment out these lines if soil moisture is not a concern
  df <- df %>%
    group_by(year) %>%
    filter(!all(is.na(soilm_p2))) %>%
    ungroup()
  
  # Apply rest of processing functions (from snotelprocessr)
  df <- df %>%
    snotel_metric() %>%
    snotel_density_calculation() %>%
    fill(soilm_p2, .direction = "down") %>%  # Down fill any NAs in soil moisture before derivative calculations
    snotel_derivative(soilm_p2)
  
  # Find DOY of peak SWE
  df <- df %>%
    group_by(year) %>%
    mutate(
      peak_swe = ifelse(any(!is.na(swe) & doy <= 182), doy[which.max(ifelse(doy <= 182, swe, NA_real_))], NA_real_)
    ) %>%
    ungroup()
  
  # Find soil moisture pulse dates for each year
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
  
  # Merge pulse results df back into the main df
  df <- df %>%
    left_join(pulse_results, by = "year") %>%
    mutate(sm_pulse_flag = if_else(doy == doy_pulse, 1, 0, missing = 0)) %>%  # Create binary column for pulse DOY
    select(-doy_pulse)  # Remove the doy_pulse column
  
  # Create output filename
  base_name <- sub("\\.csv$", "", basename(file_path))
  output_path <- file.path(output_folder, paste0(base_name, "_processed.csv"))
  
  # Write processed SNOTEL data to output folder
  write.csv(df, output_path, row.names = FALSE)
}
