#' snotel_derivative
#'
#' This function computes the derivative of a specified column.  
#'
#' @param df dataframe
#' @param input_column Column from which the derivative is being computed from
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
#'@import stats

#' @export
snotel_derivative <- function(df, col_name) {
  # Convert the input name to a symbol for use in tidyverse evaluation
  col_name_sym <- enquo(col_name)
  
  derivative_col_name <- paste0("d_", quo_name(col_name_sym))  # Create new column name based on input column
  
  df <- df %>%
    group_by(year) %>%
    mutate(!!derivative_col_name := c(0, diff(!!col_name_sym))) %>%
    ungroup()  # Ungroup after mutation to return to original structure
  
  return(df)
}

## Old function:
# snotel_derivative <- function(df, input_column) {
#   # Ensure the column name is unquoted
#   col <- rlang::ensym(input_column)
#   new_col <- paste0("d_", rlang::as_string(col))
#   
#   # Initialize the derivative column with zeroes
#   df <- df %>%
#     mutate(!!new_col := 0)
#   
#   # Calculate the derivative
#   output_deriv <- vector("list", length = nrow(df))
#   for (i in 2:nrow(df)) {
#     output_deriv[[i]] <- df[[input_column]][i] - df[[input_column]][i - 1]
#   }
#   
#   # Assign the calculated derivatives to the new column
#   df <- df %>%
#     mutate(!!new_col := c(NA, unlist(output_deriv))) %>%
#     mutate(!!new_col := if_else(is.na(!!rlang::sym(new_col)), 0, !!rlang::sym(new_col))) # Fill in beginning NA value with first derivative
#   # Convert the column to numeric
#   df <- df %>%
#     mutate(!!new_col := as.numeric(!!rlang::sym(new_col)))
#   
#   return(df)
# }
