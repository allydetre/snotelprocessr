#' snotel_metric
#'
#' This function converts values from imperial to metric system.
#'
#' @param df dataframe
#' @return Dataframe with metric values in SWE, snow depth, precip, and air temp
#' 
#'@import dplyr
#' @export

snotel_metric <- function(df){
  df <- df %>%
    mutate( swe = 2.54 * swe,
            snow_depth = 2.54 * snow_depth,
            precip = 2.54 * precip,
            airtemp = 5/9 * (airtemp - 32),
            airtemp_max = 5/9 * (airtemp_max - 32),
            airtemp_avg = 5/9 * (airtemp_avg - 32))
}

