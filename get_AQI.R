# Filename: getAQI.R
# Copyright (c) University of Washington
# License: MIT https://opensource.org/licenses/MIT (See LICENSE file.)
# Repository: https://github.com/brianhigh/web-scraping-with-r

# Get Air Quality Index (AQI) from EPA's "Airnow" site (airnow.gov).

# Clear workspace of all objects and unload all extra (non-base) packages.
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# Load packages.
if (! suppressPackageStartupMessages(require(pacman))) {
  install.packages('pacman', repos = 'http://cran.us.r-project.org')
}
pacman::p_load(dplyr, httr, rvest)

# Get a table of states and their stateid to facilitate lookup.
get_states <- function() {
  url <- 'https://airnow.gov/index.cfm'
  pg <- read_html(url)
  select_opts <- pg %>% html_nodes("select#stateid") %>% html_nodes("option")
  state <- select_opts %>% html_text()
  stateid <- select_opts %>% html_attr('value')
  df <- tibble(state = state, stateid = stateid)
  return(df)
}

# Get current AQI by city for a given stateid.
get_AQI <- function(stateid = '49') {
  url <- 'https://www.airnow.gov/index.cfm?action=airnow.print_summary'
  res <- GET(url, query = list(stateid = stateid))
  pg <- content(res, as = 'text', encoding = 'utf-8') %>% read_html()
  tbls_ls <- pg %>% html_nodes(".TblInvisible") %>% html_table(fill = TRUE)
  mat <- matrix(tbls_ls[[1]][6:287, "X1"], ncol = 6, byrow = TRUE)[, c(1, 6)]
  df <- as_tibble(mat, .name_repair = 'minimal')
  names(df)  <- c('location', 'aqi')
  return(df)
}

# Define variables.
data_dir <- 'data'
my_state <- 'Washington'

# Create data folder if it does not exist.
dir.create(data_dir, showWarnings = FALSE)

# Get a dataset of states and state IDs.
states_path <- file.path(data_dir, 'states.csv')
if (!file.exists(states_path)) {
  states <- get_states()
  write.csv(states, states_path, row.names = FALSE)
} else {
  states <- read.csv(states_path)
}

# Get the current AQI for various cities in the state of Washington.
stateid <- states %>% filter(state == my_state) %>% pull(stateid)
df <- get_AQI(stateid = stateid) %>% 
  mutate(state = my_state, datetime = Sys.time()) %>% 
  select(datetime, state, location, aqi)

# Append the current AQI data to a file to accumulate results over time.
aqi_data_path <- file.path(data_dir, paste(my_state, 'aqi.csv', sep = '_'))

# Write header if data file does not exist.
if (! file.exists(aqi_data_path)) {
  write.table(df, aqi_data_path, sep = ',', row.names = FALSE)
} else {
  # Otherwise, append data to previous records with no header.
  write.table(df, aqi_data_path, append = TRUE, sep = ',', 
              row.names = FALSE, col.names = FALSE)
}
