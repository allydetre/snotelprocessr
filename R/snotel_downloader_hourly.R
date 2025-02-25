#' snotel_downloader_hourly
#'
#' This function downloads daily SNOTEL data from one (or multiple sites) by
#' accessing the NRCS report generator.  
#'
#' @param site SNOTEL numeric site code
#' @param state State where SNOTEL station is located in
#' @param start_date Start date, in YYYY-MM-DD format
#' @param end_date End date, in YYYY-MM-DD format
#' @param save_dir Path to directory where files will be saved
#' @return CSV in desired directory with SNOTEL station data

#' @examples
#' \dontrun{
#' 
#' # Define directory to save output CSV files:
#' snotel_dir = "path/to/directory"
#' 
#' # Downloading data for one site, UT-907 for example:
#' snotel_downloader(site = "907", state = "UT", start_date = "2013-01-01", 
#' end_date = "2014-08-01", save_dir = snotel_dir)
#' 
#' # Downloading data for multiple sites:
#' 
#' # Define sites in a tibble:
#' df <- tibble(site = c(531, 335, 505, 485, 802),
#' state = c("CO" ,"CO", "CO", "CO", "CO"))
#' 
#' # Using walk2, from purrr package for a tibble/df of sites:
#' walk2(.x = df$site, .y = df$state, ~snotel_downloader(site = .x, state=.y, 
#' start_date = "2013-01-01", end_date = "2015-12-31", save_dir = snotel_dir))
#' 
#' }
#'
#'@import dplyr
#'@import stats
#' @export


#' @export
snotel_downloader_hourly <- function(site, state, start_date, end_date, save_dir){
  
  # Ensure the save directory exists, if not, create it
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Construct the URL with all parameters
  snotel_url <- paste0("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/hourly/start_of_period/",
                       site, ":",
                       state,
                       ":SNTL%257Cid=%2522%2522%257Cname/",
                       start_date, ",", end_date,
                       "/WTEQ::value,SNWD::value,PRCP::value,TOBS::value,SMS:-2:value,SMS:-4:value,SMS:-8:value,SMS:-20:value,SMS:-40:value?fitToScreen=false")
  
  # Path to save the temporary file
  #temp_file <- file.path(save_dir, paste0(site, "_", state, "_tmp.csv"))
  
  error <- httr::GET(url = snotel_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "snotel_tmp.csv"), 
                                      overwrite = TRUE))
  # read in the snotel data
  df <- utils::read.table(file.path(tempdir(),"snotel_tmp.csv"),
                          header = TRUE,
                          sep = ",",
                          stringsAsFactors = FALSE) %>%
    mutate(site_name = site,
           state_name = state)
  
  # Columns to check for missing values/NAs/zeroes in soil moisture:
  soil_moisture_columns <- c("Soil.Moisture.Percent..2in..pct.", 
                             "Soil.Moisture.Percent..4in..pct.", 
                             "Soil.Moisture.Percent..8in..pct.", 
                             "Soil.Moisture.Percent..20in..pct.", 
                             "Soil.Moisture.Percent..40in..pct.")
  
  # Check if the relevant columns exist
  existing_columns <- soil_moisture_columns[soil_moisture_columns %in% colnames(df)]
  
  # Proceed only if there are any valid soil moisture columns
  if (length(existing_columns) > 0) {
    # Check if these columns contain only NA values
    missing_data_columns <- existing_columns[sapply(df[existing_columns], function(col) all(is.na(col) | col == 0))]
    
    # If there are columns with only NA values, print the site-state information and the columns
    if (length(missing_data_columns) > 0) {
      warning(paste("The following soil moisture columns contain only NA and/or zeroes in", 
                    paste(site, state, sep = "_"), ":",
                    paste(missing_data_columns, collapse = ", ")))
    }
  }
  
  # Path to save the final CSV
  final_file <- file.path(save_dir, paste0(site, "_", state, ".csv"))
  
  # Write the dataframe to CSV
  write_csv(df, final_file)
}
