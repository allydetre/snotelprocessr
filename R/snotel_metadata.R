#' snotel_metadata
#'
#' Get metadata for SNOTEL stations in the western U.S.
#'
#' @param site_codes Numeric code(s) for one or multiple SNOTEL stations
#' @param states Two-letter state code
#' @return dataframe with metadata for desired SNOTEL sites
#' 
#' @examples
#' \dontrun{
#' # Select by site code:
#' site_1267 <- snotel_metadata(site_codes = c("1267"))
#' 
#' # Select by state:
#' state_co <- snotel_metadata(states = c("CO"))
#' 
#' # Select full metadata file for all sites/states:
#' full_metadata <- snotel_metadata(site_codes = NULL, states = NULL)
#' }
#' 
#'@import dplyr
#'@import rvest
#'@import stringr
#' @export
snotel_metadata <- function(site_codes = NULL, states = NULL) {
  # NRCS SNOTEL metadata URL
  url <- "https://wcc.sc.egov.usda.gov/nwcc/yearcount?network=sntl&counttype=statelist&state="
  
  # Read in page
  page <- read_html(url)
  
  # Extract table from page
  table <- page %>%
    html_node("table") %>%
    html_table()
  
  # Rename columns, drop ts column, pull out site codes
  table_renamed <- table %>%
    rename(
      description = "huc",
      elevation = "elev",
      network = "ntwk"
    ) %>%
    select(-ts) %>%
    mutate(site = str_extract(site_name, "\\((\\d+)\\)") %>%
             str_remove_all("[()]")) %>%
    
    # Apply filter based on provided parameters
    dplyr::filter(
      (is.null(site_codes) | site %in% site_codes) &
        (is.null(states) | state %in% states)
    )
  
  return(table_renamed)
}
