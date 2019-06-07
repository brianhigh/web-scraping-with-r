# Filename: get_cdc_elf.R
# Author: Brian High
# Date: 2019-06-07
# License: MIT https://opensource.org/licenses/MIT (See LICENSE file.)
# Repository: https://github.com/brianhigh/web-scraping-with-r

# Use the CDC ELF website to get labor force data.

# -----
# Setup
# -----

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
pacman::p_load(dplyr, httr, rvest, xts)

# ------------
# Define Query
# ------------

# Age(s): ≥ 16
# Weight in use: Composited Weight
# Labor Estimate Type: Full Time Equivalents - All Jobs
# Year(s): 2018
# Group Rows By: State, Sex

query <- list(
  LaborEstimateType = '2',
  frmId = '2',
  YearOfInterviewID = '2018',
  ageby = '2',
  ShowAllJobIOOverride = '1',
  group_by1 = '4',
  group_by2 = '6',
  weight = '2'
)

# --------
# Get Data
# --------

url <- 'https://wwwn.cdc.gov/wisards/cps/cps_estimates_results.aspx'
referer <- 'https://wwwn.cdc.gov/wisards/cps/cps_estimates.aspx'
useragent <- 'Mozilla/5.0'

res <- POST(url, 
            add_headers(Referer = referer), 
            user_agent(useragent), 
            body = query)

# -----------
# Format Data
# -----------

lst <- content(res) %>% html_nodes(".ResultsTable") %>% html_table(fill = TRUE)
df <- as_tibble(lst[[1]][, 2:4])
names(df) <- c('State', 'Sex', 'FTE')

# ----------
# Clean Data
# ----------

df <- df %>% filter(State != 'State', Sex != 'Sex', FTE != 'FTE') %>% 
  mutate_all(na_if, "") %>% filter(!is.na(Sex)) %>% 
  mutate(State = na.locf(State)) %>% 
  filter(Sex %in% c('Male', 'Female')) %>% 
  mutate(FTE = as.numeric(gsub(',', '', FTE)))

