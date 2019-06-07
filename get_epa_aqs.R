# Filename: get_epa_aqs.R
# Author: Brian High
# Date: 2019-06-06

# Use the EPA AQS API to get air quality data. This uses the API directly, as 
# an alternative to using the ebailey78/raqdm package as found on on Github, 
# which is no longer maintained and will not install into recent versions of R.

# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# Load packages.
if (!require(pacman)) {
  install.packages('pacman', repos = 'http://cran.us.r-project.org')
}
pacman::p_load(config, dplyr, jsonlite, utils)

# Import config file, if present.
config_file <- file.path("~", "epa_aqs_config.yml")
if (file.exists(config_file)) {
  config <- config::get(file = config_file)
} else {
  # Use test api email and key, if config file is not present.
  config <- list(email = "test@aqs.api", key = "test")
}

# ----------------
# Define Functions
# ----------------

# Form a URL for use with EPA AQS API.
get_url <- function(config, page = 'list/states', query_string = '') {
  url <- paste(paste0('https://aqs.epa.gov/data/api/', page, '?'), 
               paste('email', config$email, sep = '='), 
               paste('key', config$key, sep = '='), query_string, sep = '&')
  return(url)
}

# Get states.
get_states <- function(config) {
  url <- get_url(config, page = 'list/states')
  return(jsonlite::fromJSON(url)$Data)
}

# Get counties for a state code.
get_counties <- function(config, state_code) {
  url <- get_url(config, page = 'list/countiesByState', 
                 query_string = paste('state', state_code, sep = '='))
  return(jsonlite::fromJSON(url)$Data)
}

# Get classes.
get_classes <- function(config) {
  url <- get_url(config, page = 'list/classes')
  return(jsonlite::fromJSON(url)$Data)
}

# Get parameters for a class code.
get_params <- function(config, class_code) {
  url <- get_url(config, page = 'list/parametersByClass', 
                 query_string = paste('pc', URLencode(class_code), sep = '='))
  return(jsonlite::fromJSON(url)$Data)
}

# Get data for a parameter code, date range, county code, and state code.
get_data <- function(config, param_code, bdate, edate, state_code, county_code) {
  query <- list(param = param_code, bdate = bdate, edate = edate, 
                state = state_code, county = county_code)
  query_string <- paste(names(query), query, sep = "=", collapse = "&")
  url <- get_url(config, 'sampleData/byCounty', query_string)
  return(jsonlite::fromJSON(url)$Data)
}


# ------------
# Main Routine
# ------------

# To get data, we first need to look up codes for state, county, parameter 
# class and parameters...

# Get state code for Washington.
states <- get_states(config)
state_code <- states[states$value_represented == 'Washington', 'code']

# Get county code for "King" county in Washington.
counties <- get_counties(config, state_code)
county_code <- counties[counties$value_represented == "King", 'code']

# Get PM2.5 parameters for "PM2.5 Mass and QA Parameters" class.
classes <- get_classes(config)
class_code <- classes[
  classes$value_represented == "PM2.5 Mass and QA Parameters", 'code']
params <- get_params(config, class_code)

# Get "PM2.5 - Local Conditions" parameter code.
param_code <- params[
  params$value_represented == "PM2.5 - Local Conditions", 'code']

# Now we have enough codes to actually get the data we want...

# Define the start date and end date for the timespan we want data for.
# For one day's data, use the next day as the end date (edate).
bdate <- '20150501'
edate <- '20150502'

# Get PM2.5 data for King County, Washington on 2019-05-01.
df <- get_data(config, param_code, bdate, edate, state_code, county_code)

# Since we really only want one day's data right now, filter out other days.
df <- df[df$date_local == '2015-05-01', ]

