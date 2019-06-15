# Filename: get_cdc_elf.R
# Copyright (c) University of Washington
# License: MIT https://opensource.org/licenses/MIT (See LICENSE file.)
# Repository: https://github.com/brianhigh/web-scraping-with-r

# Use the CDC Employed Labor Force (ELF)  website to get labor force data. 
# While this would take longer to code than manually downloading a few datasets, 
# it could be a worthwhile approach to automate downloading many similar 
# datasets. Since the website only lets you choose a few grouping parameters, 
# you could automate collection of data for more parameters. For example, you 
# could loop through multiple states, fetching results for each state and then 
# combine them. The code below does this for 5 states in the Pacific Northwest 
# region of the USA.

# Note: While the CDC ELF website offers a button to download the data as a 
# file, we found that difficult to script, so we extract the HTML table instead.

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
pacman::p_load(dplyr, httr, rvest, xts, tidyr, forcats, ggplot2)


# ----------------
# Define Functions
# ----------------

# Get a table of states and their fips codes to facilitate lookup.
get_states <- function() {
  url <- 'https://wwwn.cdc.gov/wisards/cps/cps_estimates.aspx'
  pg <- read_html(url)
  select_opts <- pg %>% 
    html_nodes("select#StateFipsCodeID") %>% html_nodes("option")
  state <- select_opts %>% html_text() %>% trimws()
  stateid <- select_opts %>% html_attr('value')
  df <- tibble(State = state, StateFipsCodeID = stateid)
  return(df)
}

get_cdc_elf_data <- function(query) {
  # Get CDC ELF Data
  url <- 'https://wwwn.cdc.gov/wisards/cps/cps_estimates_results.aspx'
  referer <- 'https://wwwn.cdc.gov/wisards/cps/cps_estimates.aspx'
  useragent <- 'Mozilla/5.0'
  res <- POST(url, add_headers(Referer = referer), user_agent(useragent), 
              body = query)
  
  # Format Data.
  lst <- content(res) %>% 
    html_nodes(".ResultsTable") %>% 
    html_table(fill = TRUE)
  df <- as_tibble(lst[[1]][, 2:6])
  names(df) <- c('Year', 'Age', 'Male', 'Female', 'Keep')
  
  # Clean Data.
  df <- df %>% 
    filter(Year != 'Year', 
           !grepl('Age|Subtotal|:', Age), 
           !grepl('Male|FTE', Male), 
           !grepl('Female|FTE', Female))  %>% 
    mutate(Keep = ifelse(!is.na(Keep), FALSE, TRUE)) %>% 
    mutate_all(na_if, "") %>% 
    mutate(Year = na.locf(Year)) %>% 
    filter(Keep) %>% select(-Keep) %>% 
    mutate(Male = as.numeric(gsub(',', '', Male)),
           Female = as.numeric(gsub(',', '', Female)))
    df$Age <- as.factor(df$Age)
  ordered(df$Age)
  df$Age <- ordered(df$Age)
  df$StateFipsCodeID <- query$StateFipsCodeID
  
  return(df)
}

# ------------
# Main Routine
# ------------

# Define Query.

# Age(s): ≥ 16
# Weight in use: Final Weight
# Labor Estimate Type: Full Time Equivalents - All Jobs
# Non-selected variables: Default value = All
# Group Columns By: Sex
# Group Rows By: Year, Age Group(Five Years)

query <- list(
  LaborEstimateType = 2,
  frmId = 2,
  ageby = 2,
  HoursWorkedBy1 = 1,
  HoursWorkedBy2 = 1,
  TotalHoursWorkedBy = 1,
  ClassOfWorkerCrosswalkedBy = 1,
  industry83to02by = 1,
  Industry2002Job1CodesBy = 1,
  Industry2002Job2CodesBy = 1,
  occupation83to02by = 1,
  ShowAllJobIOOverride = 1,
  tab_on = 6,
  group_by1 = 2,
  group_by2 = 53,
  weight = 3
)

# Define variables.
states <- c('Alaska', 'Idaho', 'Montana', 'Oregon', 'Washington')

# Find FIPS codes for states.
state_fips <- get_states() %>% filter(State %in% states)

# Get data for each state and combine.
df <- bind_rows(lapply(1:nrow(state_fips), function(x) {
  # Add state FIPS code as a query parameter.
  query$StateFipsCodeID <- state_fips$StateFipsCodeID[x]
  get_cdc_elf_data(query)
}))

# Merge with FIPS codes dataframe to get state name.
df <- df %>% inner_join(state_fips, by = 'StateFipsCodeID') %>% 
  select(-StateFipsCodeID)

# Reshape Male and Female into a new Gender variable. Reverse Age factor order. 
df <- df %>% gather(key = 'Gender', value = 'Count', Male, Female) %>% 
  mutate(Age = fct_rev(Age))

# Calculate the percent of workers aged over 65 years by state, gender, & year.
df_65_and_Older <- df %>% 
  mutate(Age = ifelse(
    Age %in% c("75+", "70-74", "65-69"), 'Age_65_Plus', 'Under_65')) %>%
  group_by(State, Gender, Year, Age) %>% 
  summarise(Count = sum(Count)) %>% 
  spread(key = Age, value = Count) %>% 
  group_by(State, Gender, Year) %>% 
  mutate(Pct_65_Plus = 100 * Age_65_Plus / (Age_65_Plus + Under_65))

# Make a plot.
ggplot(df_65_and_Older, aes(x = Year, y = Pct_65_Plus, group = Gender)) + 
  geom_line(aes(color = Gender)) + 
  facet_grid(rows = vars(State)) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle(label = "Percentage of workers age 65 or older in five PNW states", 
          subtitle = "Data Source: CDC Employed Labor Force") + 
  ylab("Percent (%) of workers age 65 or older") + xlab("Year")
