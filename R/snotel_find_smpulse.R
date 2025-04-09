#' snotel_detect_smpulse
#'
#' Soil moisture pulse detection from daily SNOTEL metrics of soil moisture. 
#' The soil moisture pulse serves as a proxy for melt output from the snowpack.
#'
#' @param df input processed SNOTEL df
#' @param soil_moisture_col column containing soil moisture of desired depth
#' @param doy_col column containing DOY
#' @param year_col column containing year
#' @param swe_col column containing SWE
#' @param min_swe_window_before define number of days before peak SWE that the algorithm should search for a pulse
#' @param max_swe_window_after define number of days after peak SWE that the algorithm should search for a pulse
#' @param variability_multiplier define standard deviation multiplier (higher multipliers will look for bigger pulses, lower multipliers will look for smaller pulses)
#' @param derivative_length define forward-looking derivative length (longer length will smooth the derivaive more, while shorter length will smooth the derivative less)
#' @return dataframe with DOY of soil moisture pulse for all available years
#' 
#' @examples
#' \dontrun{
#' 
#'  # Find pulses for a given SNOTEL processed df:
#'  # Change params as needed!
#' pulse_results <- snotel_detect_smpulse(
#'df = df, 
#'soil_moisture_col = "soilm_p2", 
#'doy_col = "doy", 
#'year_col = "year", 
#'swe_col = "swe", 
#'min_swe_window_before = 70, 
#'max_swe_window_after = 30,
#'variability_multiplier = 1.25, 
#'derivative_length = 2)
#' 
#' }
#'
#'@import dplyr
#' @export

snotel_detect_smpulse <- function(df, soil_moisture_col, doy_col, year_col, swe_col,
                           min_swe_window_before, max_swe_window_after,
                           variability_multiplier, derivative_length) {
  
  # Internal function to find pulse date for individual years
  find_pulse_in_year <- function(sub_df) {
    # Extract relevant columns
    soil_moisture_values <- sub_df[[soil_moisture_col]]
    doy_values <- sub_df[[doy_col]]
    year_value <- unique(sub_df[[year_col]])
    swe_values <- sub_df[[swe_col]]
    
    # Ensure enough data points for derivative calculation
    if (length(soil_moisture_values) < derivative_length + 1) {
      return(tibble(year = year_value, doy_pulse = as.numeric(NA)))
    }
    
    # Calculate forward derivative
    calculated_derivative <- dplyr::lead(soil_moisture_values, derivative_length) - soil_moisture_values
    
    # Calculate dynamic threshold
    dynamic_threshold <- variability_multiplier * stats::sd(calculated_derivative, na.rm = TRUE)
    
    # Determine peak SWE DOY before DOY 182 to avoid late-season noise
    if (all(is.na(swe_values)) || max(doy_values) < 182) {
      return(tibble(year = year_value, doy_pulse = as.numeric(NA)))
    }
    peak_swe_doy <- doy_values[which.max(swe_values[doy_values <= 182])]
    
    # Define time window around peak SWE DOY
    window_start <- peak_swe_doy - min_swe_window_before
    window_end <- peak_swe_doy + max_swe_window_after
    
    # Filter indices within the SWE time window
    valid_indices <- which(doy_values >= window_start & doy_values <= window_end)
    
    # Subset the derivative and DOY values to only those within the SWE window
    valid_derivatives <- calculated_derivative[valid_indices]
    valid_doy_values <- doy_values[valid_indices]
    
    # Find the first instance where the derivative exceeds the threshold
    pulse_index <- which(valid_derivatives >= dynamic_threshold)[1]
    
    if (!is.null(pulse_index) && length(pulse_index) > 0) {
      return(tibble(year = year_value, doy_pulse = valid_doy_values[pulse_index] + 1))
    }
    
    # No valid instance found
    return(tibble(year = year_value, doy_pulse = as.numeric(NA)))
  }
  
  # Apply the function year-wise
  results <- df %>%
    group_by(across(all_of(year_col))) %>%
    do(find_pulse_in_year(.)) %>%
    ungroup()
  
  return(results)
}