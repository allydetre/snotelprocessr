#' snotel_rename_col_hourly
#'
#' This function renames the columns in the raw SNOTEL CSV download so that
#' they are more friendly for R and easier to understand. This function also
#' creates new columns for various date/time metrics that will be required
#' later on in general scripting.
#'
#' @param df Input SNOTEL dataframe
#' @param rename_map named vector where the names are the new column names and 
#' the values are the existing column names. If custom rename_map is desired,
#' arrange it in "newcolumn" = "oldcolumn" format.
#' @return Dataframe with renamed columns
#' 
#'@import dplyr
#'@import lubridate
#' @export

snotel_rename_col_hourly <- function(df, rename_map=NULL) {
  
  # Use default rename_map if not provided
  if (is.null(rename_map)) {
    rename_map <- c(
      "date" = "Date",
      "swe" = "Snow.Water.Equivalent..in.",
      "snow_depth" = "Snow.Depth..in.",
      "precip" = "Precipitation.Increment..in.",
      "airtemp" = "Air.Temperature.Observed..degF.",
      "aitemp_max" = "Air.Temperature.Maximum..degF.",
      "airtemp_avg" = "Air.Temperature.Average..degF.",
      "soilm_p2" = "Soil.Moisture.Percent..2in..pct.",
      "soilm_p4" = "Soil.Moisture.Percent..4in..pct.",
      "soilm_p8" = "Soil.Moisture.Percent..8in..pct.",
      "soilm_p20" = "Soil.Moisture.Percent..20in..pct.",
      "soilm_p40" = "Soil.Moisture.Percent..40in..pct.",
      "site_code" = "site_name",
      "state" = "state_name"
    )
  }
  
  # Print original dataframe column names - uncomment to check
  df_columns <- colnames(df)
  # print("Original dataframe column names:")
  # print(df_columns)
  # 
  # # Print rename map
  # print("Rename map:")
  # print(rename_map)
  
  # Identify exact matches and mismatches
  map_keys <- names(rename_map)
  map_values <- rename_map
  
  # Compare columns with case insensitivity
  matching_columns <- map_values[tolower(map_values) %in% tolower(df_columns)]
  names(matching_columns) <- map_keys[tolower(map_values) %in% tolower(df_columns)]
  
  # Error checking:
  # print("Matching columns:")
  # print(names(matching_columns))
  # print("Non-matching columns:")
  # print(map_keys[!tolower(map_values) %in% tolower(df_columns)])
  # 
  # # Debugging: Print valid rename map
  # print("Valid rename map:")
  # print(matching_columns)
  
  if (length(matching_columns) > 0) {
    df <- df %>%
      rename(!!!matching_columns)
  }
  
  # create appropriate columns for time/date:
  df <- df %>%
    rename(datetime = date)%>%
    mutate(datetime = ymd_hm(datetime),
           date = as.Date(datetime),
           year = year(datetime),
           hour = hour(datetime),
           doy = yday(datetime))
  
  # Filter for only January through August:
  df <- df %>%
    dplyr::filter(month(date) == 1 & day(date) >= 1 |    # January 1st or later
             month(date) <= 7 |                   # Up to July
             month(date) == 8 & day(date) <= 2)  # August 2nd or earlier
  
  return(df)
}
