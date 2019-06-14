# Filename: get_epa_aqs.R
# Copyright (c) University of Washington
# License: MIT https://opensource.org/licenses/MIT (See LICENSE file.)
# Repository: https://github.com/brianhigh/web-scraping-with-r

# Use the EPA AQS API to get air quality data. This uses the API directly, as 
# an alternative to using the ebailey78/raqdm package as found on on Github, 
# which is no longer maintained and will not install into recent versions of R.

# -----
# Setup
# -----

# Clear workspace of all objects and unload all extra (non-base) packages.
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
pacman::p_load(config, jsonlite, ggplot2)

# ----------------
# Define Functions
# ----------------

# Get configuration parameters.
get_config <- function(config_file = file.path("~", "epa_aqs_config.yml"),
                       email = "test@aqs.api", key = "test") {
  # Import config file, if present.
  if (file.exists(config_file)) {
    config <- config::get(file = config_file)
  } else {
    # Use test api email and key, if config file is not present.
    config <- list(email = email, key = key)
  }
  return(config)
}

# Create a URL.
create_url <- function(base_url, page, query_string = '') {
  return(paste(paste0(base_url, page, '?'), query_string, sep = '&'))
}

# Get data from API.
get_data <- function(base_url = 'https://aqs.epa.gov/data/api/', page, query) {
  query_string <- paste(names(query), query, sep = "=", collapse = "&")
  url <- create_url(base_url, page, query_string)
  return(jsonlite::fromJSON(url)$Data)
}

# Get sample data by county and parameter from API.
get_sample_data_by_county_and_parameter <-
  function(config, state_name, county_name, class_name, param_name, 
           start_date, end_date) {
  # Define lists to use for constructing the query string.
  base_query <- list(email = URLencode(config$email), 
                     key = URLencode(config$key))
  location_query <- list()
  param_query <- list()
  date_query <- list()
    
  # Get state code.
  states <- get_data(page = 'list/states', query = base_query)
  location_query$state <- URLencode(
    states[states$value_represented == state_name, 'code'])
  
  # Get county code.
  query <- append(base_query, location_query)
  counties <- get_data(page = 'list/countiesByState', query = query)
  location_query$county <- URLencode(
    counties[counties$value_represented == county_name, 'code'])
  
  # Get parameter class code.
  classes <- get_data(page = 'list/classes', query = base_query)
  param_query$pc <- URLencode(
    classes[classes$value_represented == class_name, 'code'])
  
  # Get parameter code.
  query <- append(base_query, param_query)
  params <- get_data(page = 'list/parametersByClass', query = query)
  param_query$param <- URLencode(
    params[params$value_represented == param_name, 'code'])
  
  # Define the start date and end date for the timespan we want data for.
  # For one day's data, use the next day as the end date (edate).
  date_query$bdate <- URLencode(start_date)
  date_query$edate <- URLencode(end_date)
  
  # Combine query variables
  query <- append(base_query, append(location_query, date_query))
  query$param <- param_query$param
  
  # Get sample data.
  return(get_data(page = 'sampleData/byCounty', query = query))
}

# ------------
# Main Routine
# ------------

# Get sample data from API.
df <- get_sample_data_by_county_and_parameter(
  config = get_config(),
  state_name = 'Washington',
  county_name = 'King',
  class_name = 'PM2.5 Mass and QA Parameters',
  param_name = 'PM2.5 - Local Conditions',
  start_date = '20180819',
  end_date = '20180823'
)

if (class(df) == "data.frame" & nrow(df) > 0) {

  # Clean up data.

  # Combine date and time to make a timestamp variable (for plotting).
  df$datetime <- as.POSIXct(paste(df$date_local, df$time_local, sep = " "))
  
  # Remove extra variables and rows with NAs.
  df <- df[, c('datetime', 'sample_measurement')]
  df <- df[complete.cases(df),]
  
  # Get mean of PM2.5 for each timestamp.
  df <- aggregate(sample_measurement ~ datetime, df, mean)
  
  # Plot data.
  ggplot(df, aes(x = datetime, y = sample_measurement)) + geom_point() + 
    stat_smooth(method = 'loess') + ylab('PM2.5') + 
    ggtitle('Hourly PM2.5 from 2018-08-19 to 2018-08-23 in King County, WA', 
            subtitle = 'Data source: EPA AQS Datamart, https://aqs.epa.gov/api')
}