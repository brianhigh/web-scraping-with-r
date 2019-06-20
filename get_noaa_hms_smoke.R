# Filename: get_noaa_hms_smoke.R
# Copyright (c) University of Washington
# License: MIT https://opensource.org/licenses/MIT (See LICENSE file.)
# Repository: https://github.com/deohs/coders

# Get daily smoke shapefiles from NOAA by year. We automate this task to 
# avoid the tedium of downloading thousands of individual files manually.


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
pacman::p_load(rvest, httr, pbapply, stringi)


# ----------------
# Define functions
# ----------------

create_base_url <- function(year) {
  # Construct a base URL for file download given a year.
  domain <- "satepsanone.nesdis.noaa.gov"
  URL <- "http://" %s+% domain %s+% "/pub/volcano/FIRE/HMS_ARCHIVE/" %s+%
           year %s+% "/GIS/SMOKE/"
  return(URL)
}

get_file_list <- function(URL, regexp) {
  # Get a list of matching URLs from a file index using a CSS selector.
  files <- read_html(URL) %>% html_nodes("a") %>% html_attr("href") %>% 
    grep(regexp, ., value = TRUE)
  return(files)
}

get_files <- function(URL, files, data_dir) {
  # Create destination folder if it does not already exist.
  dir.create(data_dir, showWarnings = FALSE)
  if (dir.exists(data_dir)) {
    # Get only those files we do not already have.
    invisible(pbsapply(files, function(file) {
      file_path <- file.path(data_dir, file)
      if (!file.exists(file_path)) {
        GET(URL %s+% file, write_disk(file_path))
      }
    }))
  }
}

download_data <- function(year, data_dir = file.path('data', 'smoke')) {
  # Construct a base URL for file download given a year.
  base_url <- create_base_url(year)
  
  # Define a regular expression to match the files we want to download.
  regexp <- 'hms_smoke[0-9]{4}0[5-9]{1}[0-9]{2}\\.(dbf|shp|shx)\\.gz$'
  
  # Get a list of files matching our regular expression.
  files <- get_file_list(base_url, regexp)
  
  # Get the files.
  res <- get_files(base_url, files, data_dir)
}


# ------------
# Main routine
# ------------

# Define variables.
years <- seq(2006, 2017)

# Get data. This will take 5+ minutes per year, depending on network speed.
res <- lapply(years, download_data)
