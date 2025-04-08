### SNOTEL download raw data ###

# *Important to note: run this script first for downloading raw SNOTEL data. 
# Once downloaded, then run processing script

################################################################################

library(tidyverse)
library(snotelprocessr)
library(purrr)

### Download daily data:

# Define directory to save output CSV files:
snotel_dir = "path/to/directory"

# Downloading data for one site, UT-907 for example:
snotel_downloader_daily(site = "907", state = "UT", start_date = "2013-01-01",
end_date = "2014-08-01", save_dir = snotel_dir)

# Downloading data for multiple sites:

# Define sites in a tibble:
df <- tibble(site = c(531, 335, 505, 485, 802),
state = c("CO" ,"CO", "CO", "CO", "CO"))

# Using walk2, from purrr package for a tibble/df of sites:
walk2(.x = df$site, .y = df$state, ~snotel_downloader_daily(site = .x, state=.y,
start_date = "2013-01-01", end_date = "2015-12-31", save_dir = snotel_dir))

### ### ### ### ### 

### Download hourly data:

# Define directory to save output CSV files:
snotel_dir = "path/to/directory"

# Downloading data for one site, UT-907 for example:
snotel_downloader_hourly(site = "907", state = "UT", start_date = "2013-01-01",
end_date = "2014-08-01", save_dir = snotel_dir)

# Downloading data for multiple sites:

# Define sites in a tibble:
df <- tibble(site = c(531, 335, 505, 485, 802),
state = c("CO" ,"CO", "CO", "CO", "CO"))

# Using walk2, from purrr package for a tibble/df of sites:
walk2(.x = df$site, .y = df$state, ~snotel_downloader_hourly(site = .x, state=.y,
start_date = "2013-01-01", end_date = "2015-12-31", save_dir = snotel_dir))
