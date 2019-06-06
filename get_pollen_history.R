# Filename: get_pollen_history.R
# Author: Brian High
# Date: 2019-06-06

if (!require(pacman)) install.packages('pacman')
pacman::p_load(RCurl, jsonlite)

# Get historic pollen count given a zipcode and number of days.
get_pollen <- function(zip = 98104, days = '30') {
  base_url <- 'https://www.pollen.com'
  useragent <- 'Mozilla/5.0'
  referer <- paste0(base_url, '/forecast/historic/pollen/', zip)
  URL <- paste0(base_url, '/api/forecast/historic/pollen/', zip, '/', days)
  json_data <- getURL(URL, referer = referer, useragent = useragent)
  pollen <- fromJSON(json_data)$Location$periods
  pollen$Period <- as.Date(gsub('T.*$', '', pollen$Period))
  pollen$zip <- zip
  names(pollen) <- c('date', 'pollen_index', 'zip')
  pollen
}

get_pollen(zip = '98105', days = '60')
