## Defining global variables that are column names used in dplyr pipelines ##
# made to suppress notes about global variables in package dev #

utils::globalVariables(c(
  ".", ":=", "airtemp", "airtemp_avg", "airtemp_max", "datetime", "precip",
  "runmed", "site", "site_name", "snow_depth", "state", "swe", "ts"
))
