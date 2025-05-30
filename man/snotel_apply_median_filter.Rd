% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snotel_apply_median_filter.R
\name{snotel_apply_median_filter}
\alias{snotel_apply_median_filter}
\title{snotel_apply_median_filter}
\usage{
snotel_apply_median_filter(
  df,
  column_name,
  window_size,
  cleanup_dates = NULL,
  new_column_name
)
}
\arguments{
\item{df}{dataframe}

\item{column_name}{Column that is being median filtered}

\item{window_size}{number of data points used to compute the median. Can also be though of as n.}

\item{cleanup_dates}{List of dates in YYYY-MM-DD format where data values need to be NA for plotting purposes.}

\item{new_column_name}{Name of new column with median filtered values (output of this function)}
}
\value{
Median filtered column
}
\description{
This function  takes the dataframe, the column name (unquoted), the window 
size for the median filter, and a list of dates to clean up, and runs a 
median filter over it.
}
\examples{
\dontrun{
# Load your dataframe
data(snotel_000)

# First denote dates that need to be set to NA for plotting purposes, if applicable:
cleanup_dates <- c("2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01",
                  "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01", 
                  "2023-01-01","2024-01-01", "2013-08-02", "2014-08-02", "2015-08-02", 
                  "2016-08-02", "2017-08-02", "2018-08-02", "2019-08-02", "2020-08-02", 
                  "2021-08-02", "2022-08-02", "2023-08-02", "2024-08-02")

# Call and apply median filter function to soil moisture column:
snotel_000 <- snotel_apply_median_filter(snotel_000, soilm_p2, 53, cleanup_dates, 
new_column_name = "med_soilm53_cleaned")
}

}
