# Filename: get_wa_wqi.R
# Copyright (c) University of Washington
# License: MIT https://opensource.org/licenses/MIT (See LICENSE file.)
# Repository: https://github.com/brianhigh/web-scraping-with-r

# Download Annual 2013 Water Quality Index Scores file, import, clean up & plot.
# https://catalog.data.gov/dataset?q=washington+state+annual+freshwater+coliform

# From: https://catalog.data.gov/dataset/annual-2013-water-quality-index-scores-4d1fd
# "For temperature, pH, oxygen, and fecal coliform bacteria, the WQI is based 
# on criteria in Washington’s Water Quality Standards, WAC 173-201A."


# ---------- Setup --------------

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
pacman::p_load(dplyr, tidyr, readr, ggmap)


# ---------- Get data --------------

# Import the data.
url <- 'https://data.wa.gov/api/views/h7j9-vgr3/rows.csv?accessType=DOWNLOAD'
wa_wqi <- suppressMessages(read_csv(url))

# Look at the structure of the data.
wa_wqi

# Parse location column to get latitude and longitude columns.
wa_wqi <- wa_wqi %>% 
  mutate(Location.1 = gsub('POINT |[()]', '', `Location 1`)) %>%
  separate(Location.1, c('lon', 'lat'), ' ', convert = TRUE)
  

# Look at the location data.
options(pillar.sigfig = 7)
wa_wqi %>% select(`Location 1`, lon, lat)

# ---------- Create map --------------

# Define a boundary box.
height <- max(wa_wqi$lat) - min(wa_wqi$lat)
width <- max(wa_wqi$lon) - min(wa_wqi$lon)
bbox <- c(
  min(wa_wqi$lon) - 0.15 * width,
  min(wa_wqi$lat) - 0.15 * height,
  max(wa_wqi$lon) + 0.15 * width,
  max(wa_wqi$lat) + 0.15 * height
)
names(bbox) <- c('left', 'bottom', 'right', 'top')

# Make a map base layer of "Stamen" tiles.
map <- suppressMessages(
  get_stamenmap(bbox, zoom = 8, maptype = "toner-background"))

# Make the map image from the tiles.
g <- ggmap(map, darken = c(0.3, "white")) + theme_void() 

# Show the map.
g + geom_point(aes(x = lon, y = lat, fill = WQIFC), 
               data = wa_wqi, pch = 21, size = 3) + 
  scale_fill_gradient(name = "FCI", low = "green", high = "red") + 
  ggtitle(label = paste("Washington State", 
                        "Freshwater Fecal Coliform Index (FCI)", sep = " "),
          subtitle = paste("Source: River and Stream Monitoring Program,", 
                           "WA State Department of Ecology (2013)")) +
  theme(legend.position = c(.98, .02), legend.justification = c(1, 0)) 
