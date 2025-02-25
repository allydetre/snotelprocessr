### Getting SNOTEL 50% SWE threshold for Sentinel-1 filtering ###

# we need to find the dates where SWE reaches 50% of peak SWE (heading towards peak SWE)
# and then the date where SWE goes back down and reaches 50% of peak SWE on the other side of peak SWE
# output these in DOY to a df for all stations, we will need peak SWE for all of these stations

################################################################################

library(tidyverse)

# Define the folder with processed SNOTEL data
folder_path <- "path/to/processed/snotel/data"

# Get a list of all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty df to store results
results <- tibble(
  station_name = character(), 
  year = character(), 
  peak_swe = numeric(), 
  peak_swe_value = numeric(),
  first_50p_swe = numeric(),
  first_50p_swe_value = numeric(),
  last_50p_swe = numeric(),
  last_50p_swe_value = numeric()
)

# Loop through each CSV file
for (file in csv_files) {
  # Read the CSV file
  df <- read_csv(file, show_col_types = FALSE)
  
  # Make sure year is a character and peak_swe is numeric!
  df <- df %>%
    mutate(year = as.character(year),
           peak_swe = as.numeric(peak_swe))
  
  # Extract station name from file name
  station_name <- str_extract(basename(file), "^[^_]+_[^_]+")
  
  # Process df to find peak SWE and the first/last 50% SWE threshold
  file_results <- df %>%
    select(year, peak_swe, swe, doy) %>%
    filter(!is.na(peak_swe)) %>%            
    group_by(year) %>%                        
    summarize(
      peak_swe = first(peak_swe),             
      peak_swe_value = swe[doy == first(peak_swe)], # SWE at peak_swe DOY
      first_50p_swe = doy[min(which(swe >= 0.5 * peak_swe_value))], # First DOY where SWE >= 50% of peak SWE
      first_50p_swe_value = swe[doy == first_50p_swe],  # SWE at first_50p_swe
      last_50p_swe = doy[max(which(swe >= 0.5 * peak_swe_value))],  # Last DOY where SWE >= 50% of peak SWE
      last_50p_swe_value = swe[doy == last_50p_swe],  # SWE at last_50p_swe
      .groups = "drop"
    ) %>%
    mutate(station_name = station_name,
           peak_swe_value = as.numeric(peak_swe_value),
           first_50p_swe = as.numeric(first_50p_swe),
           first_50p_swe_value = as.numeric(first_50p_swe_value),
           last_50p_swe = as.numeric(last_50p_swe),
           last_50p_swe_value = as.numeric(last_50p_swe_value))
  
  # Append to the results df
  results <- bind_rows(results, file_results)
}

# Export the combined results to a CSV
write_csv(results, "output/path")