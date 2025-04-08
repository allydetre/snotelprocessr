#' snotel_medfilter
#'
#' This function runs a median filter on a column in a dataframe.  
#'
#' @param x column in specified dataframe
#' @param n median filtering factor (a number)
#' @return median filtered column in specified dataframe
#' 
#' @examples
#' \dontrun{
#' # Load your dataframe
#' data(snotel_000)
#' 
#' # Run median filter on specified column, here we are choosing density with n = 3:
#' CO_335_sample %>%
#'   mutate(density_medfilter = snotel_medfilter(density_calc, n = 3))
#' 
#' }
#' 
#'@import dplyr
#' @export

snotel_medfilter <- function(x,n){
  runmed(x,n)
}
