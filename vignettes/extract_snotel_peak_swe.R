### Export peak SWE DOY for processed SNOTEL df ###

# *Important to note: this script requires processed SNOTEL df
# The output is a single CSV with peak SWE DOY for all stations/years

library(tidyverse)



# Define the folder containing the CSV files
folder_path <- "path/to/processed/snotel/df"

# Get a list of all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty dfframe to store results
results <- tibble(
  station_name = character(), 
  year = character(), 
  peak_swe = numeric(), 
  peak_swe_value = numeric()
)

# Loop through each CSV file
for (file in csv_files) {
  # Read the CSV file
  df <- read_csv(file, show_col_types = FALSE)
  
  # Make year a character
  df <- df %>%
    mutate(year = as.character(year),
           peak_swe = as.numeric(peak_swe))
  
  # Extract station name from file name
  station_name <- str_extract(basename(file), "^[^_]+_[^_]+")
  
  # Process df to find peak SWE values
  file_results <- df %>%
    select(year, peak_swe, swe, doy) %>%
    filter(!is.na(peak_swe)) %>%                
    group_by(year) %>%                         
    summarize(
      peak_swe = first(peak_swe),              
      peak_swe_value = swe[doy == first(peak_swe)], 
      .groups = "drop"
    ) %>%
    mutate(station_name = station_name,
           peak_swe_value = as.numeric(peak_swe_value))
  
  # Append to the results dfframe
  results <- bind_rows(results, file_results)
}


# Export the combined results to a CSV
write_csv(results, "output/path")