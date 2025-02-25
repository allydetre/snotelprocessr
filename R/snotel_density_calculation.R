#' snotel_density_calculation
#'
#' This function calculates density from SWE and snow depth.  
#'
#' @param df dataframe
#' @return column containing calculated density values

#' @examples
#' \dontrun{
#' # Load your dataframe
#' data(snotel_000)
#' 
#' # Calculate density
#' snotel_000 <- snotel_000 %>%
#'   snotel_density_calculation()
#' }
#'
#'@import dplyr
#'@import stats

#' @export

snotel_density_calculation <- function(df, swe, snow_depth){
  df <- df %>%
    mutate(density_calc = ((df$swe / df$snow_depth)*1000))
  
}
