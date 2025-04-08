#' snotel_derivative
#'
#' This function computes the derivative of a specified column.  
#'
#' @param df dataframe
#' @param col_name Column from which the derivative is being computed from
#' @return Column with computed derivative

#' @examples
#' \dontrun{
#' # Load dataframe
#' data(snotel_000)
#' 
#' # Calculate derivative for 'soilm_p2' column
#' snotel_000 <- snotel_000 %>%
#'   snotel_derivative(soilm_p2)
#' }
#'@import dplyr

#' @export
snotel_derivative <- function(df, col_name) {
  # Convert the input name to symbol
  col_name_sym <- enquo(col_name)
  
  derivative_col_name <- paste0("d_", quo_name(col_name_sym)) 
  
  df <- df %>%
    group_by(year) %>%
    mutate(!!derivative_col_name := c(0, diff(!!col_name_sym))) %>%
    ungroup()
  
  return(df)
}