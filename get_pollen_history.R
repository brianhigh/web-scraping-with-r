# Filename: get_pollen_history.R
# Author: Brian High
# Date: 2019-06-06

# Load packages.
if (!require(pacman)) {
  install.packages('pacman', repos = 'http://cran.r-project.org')
}
pacman::p_load(RCurl, jsonlite)

# Get historic pollen index given a zipcode and number of days.
get_pollen <- function(zip = 98104, days = '30') {
  # Define variables.
  base_url <- 'https://www.pollen.com'
  forecast_path <- 'forecast/historic/pollen'
  referer <- paste(base_url, forecast_path, zip, sep = '/')
  useragent <- 'Mozilla/5.0'
    
  # Get data.
  URL <- paste(base_url, 'api', forecast_Path, zip, days, sep = '/')
  json_data <- getURL(URL, referer = referer, useragent = useragent)
  pollen <- fromJSON(json_data)$Location$periods
  
  # Clean data.
  pollen$Period <- as.Date(gsub('T.*$', '', pollen$Period))
  pollen$zip <- zip
  names(pollen) <- c('date', 'pollen_index', 'zip')
  
  return(pollen)
}

# Get historical pollen index for a specific zip code and number of days.
get_pollen(zip = '98105', days = '60')
