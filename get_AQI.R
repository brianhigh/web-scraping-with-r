# Filename: getAQI.R
# Author: Brian High
# Date: 2019-06-06
# License: MIT https://opensource.org/licenses/MIT (See LICENSE file.)
# Repository: https://github.com/brianhigh/web-scraping-with-r

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
  df <- as_tibble(mat)
  names(df)  <- c('location', 'current_aqi')
  return(df)
}

# Get the current AQI for various cities in the state of Washington.
states <- get_states()
stateid <- states %>% filter(state == "Washington") %>% pull(stateid)
get_AQI(stateid = stateid)
