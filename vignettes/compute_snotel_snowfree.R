### Generating snow-free dates for SNOTEL stations ###

# *Important to note: this script should be ran on processed SNOTEL data
# This script will generate snow-free dates for each SNOTEL station based on snow depth

################################################################################

library(tidyverse)
library(readr)

# Define the input folder containing processed SNOTEL data
folder_path <- "path/to/processed/snotel/data"

# Get a list of all CSV files
csv_files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)

# Initialize an empty dataframe to store results
snotel_snowfree <- data.frame()

# Loop through each CSV
for (file in csv_files) {
  
  # Extract station name from filename
  station_name <- gsub("_processed.csv", "", basename(file))
  
  # Read the CSV and handle any parsing issues
  df <- read_csv(file, col_types = cols(.default = "c")) %>%
    mutate(
      date = ymd(date),                
      year = year(date),
      doy = yday(date),
      snow_depth = as.numeric(snow_depth)
    ) %>%
    arrange(date)  # Ensure chronological order of dates
  
  # Check for any parsing issues
  if (any(is.na(df$snow_depth))) {
    warning(paste("NA values detected in snow_depth for", station_name, "– Check data formatting"))
  }
  
  # Compute first_snow and snow_free DOY
  df_summary <- df %>%
    group_by(year) %>%
    summarize(
      # Find the first DOY where snow is present (> 0)
      first_snow = ifelse(any(snow_depth > 0, na.rm = TRUE), doy[min(which(snow_depth > 0))], NA_integer_),
      .groups = "drop"
    ) %>%
    # Join df back to filter for snow-free days AFTER first snow appears
    left_join(df, by = "year") %>%
    filter(doy >= first_snow & snow_depth == 0) %>%
    group_by(year) %>%
    summarize(
      station = first(station_name),
      first_snow = first(first_snow),  # Keep first_snow from prev computation
      snow_free = ifelse(n() > 0, first(doy), NA_integer_),  # Get first snow-free date AFTER first_snow
      .groups = "drop"
    )
  
  # Append results to the final dataframe
  snotel_snowfree <- bind_rows(snotel_snowfree, df_summary)
}

# write file to CSV
write_csv(snotel_snowfree, "path/to/output/csv")
