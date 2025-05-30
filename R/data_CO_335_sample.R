#' Processed SNOTEL data for site CO_335 for year 2023
#'
#'
#' @format A data frame with daily SNOTEL data comprised of 214 rows and 21 variables:
#' \describe{
#'   \item{date}{Date, in MM/DD/YYYY format}
#'   \item{swe}{Snow Water Equivalent (cm)}
#'   \item{snow_depth}{Snow depth (cm)}
#'   \item{precip}{Precipitation (cm)}
#'   \item{airtemp}{Air temperature (°C)}
#'   \item{airtemp_max}{Air temperature (daily maximum, °C)}
#'   \item{airtemp_avg}{Air temperature (daily average, °C)}
#'   \item{soilm_p2}{Soil Moisture at 2" depth (\%)}
#'   \item{soilm_p4}{Soil Moisture at 4" depth (\%)}
#'   \item{soilm_p8}{Soil Moisture at 8" depth (\%)}
#'   \item{soilm_p20}{Soil Moisture at 20" depth (\%)}
#'   \item{soilm_p40}{Soil Moisture at 40" depth (\%)}
#'   \item{site_code}{SNOTEL station site code}
#'   \item{state}{SNOTEL station state}
#'   \item{year}{Year}
#'   \item{doy}{Day of Year}
#'   \item{month}{Month}
#'   \item{density_calc}{Calculated density (kg m-3)}
#'   \item{d_soilm_p2}{Derivative soil moisture (2" depth)}
#'   \item{peak_swe}{Peak SWE DOY}
#'   \item{sm_pulse_flag}{Soil moisture pulse flag (1 = pulse date, 0 = no pulse)}
#' }
#' @source Internal dataset, originally downloaded from NRCS database
"CO_335_sample"
