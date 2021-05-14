# Libraries
library(dplyr)
library(tidyr)
library(stringr)
library(googlesheets4)

# --- Helper functions --------------------------------------------------------

clean_name <- function(name) {
  
  # Return a clean name for CCMF data column
  
  name_out <- name %>%
    tolower() %>%
    str_replace_all("\\s", "_") %>%
    str_replace_all("-", "_") %>%
    str_replace_all("day_of_month", "day") %>%
    str_replace_all("city_or_region", "city") %>%
    str_replace_all("\\(s\\)", "") %>%
    str_replace_all("gender_of_victim", "gender") %>%
    str_replace_all("type_of_incident", "incident_type")
  
  return(name_out)
}

# --- Read CCMF main data table from Google Sheet -----------------------------

# CCMF spreadsheet URL
ccmf_url <- "https://docs.google.com/spreadsheets/d/11jB7OgjugRAOJTVpF1Kw9mQyCwZtsBJGOcAMSOmwtWY/edit?usp=sharing"

# Access sheets as an anonymous user
gs4_deauth()

# Error handling in case it fails to connect to the Google Sheet on the first try
test <- httr::RETRY("GET", ccmf_url, times=3)

if (test$status_code == 404) {
  stop("Error 404 - Unable to Connect to Google Sheets")
}

# Main data table (incident records)
ccmf_in <- read_sheet(
  ccmf_url,
  sheet = "Data"
  # col_types = "i-ccDiii-ccccc--cccccc---"
)

# Reference table (categories)
ccmf_categories <- read_sheet(
  ccmf_url,
  sheet = "Categories"
) %>%
  mutate(field = clean_name(field))

# --- Data processing ---------------------------------------------------------

# Reference table with 2-letter province codes, to join with CCMF table
provinces <- read.csv("./data/ref/pop_prov_2016.csv")

# Process data
ccmf_out <- ccmf_in %>%
  
  # Clean column names
  rename_with(clean_name) %>%
  
  # Remove unnecessary columns
  select(-c("incident_year_month", "data_entry_warnings")) %>%
  
  # Drop empty rows
  drop_na(incident_id) %>%
  
  # Strip any leading/trailing whitespace
  mutate_if(is.character, str_trim) %>%
  
  # Fill missing incident types
  mutate(incident_type = replace_na(incident_type, "Other")) %>%
  
  # Construct incident date from year-month-day components, filling any
  # missing days with 1
  mutate(incident_date = paste(sprintf("%d", incident_year),
                               sprintf("%02d", incident_month),
                               sprintf("%02d", 
                                       replace_na(incident_day, 1)),
                               sep="-")) %>%
  
  # Get 2-letter province codes from provinces reference table
  left_join(provinces %>% select(province, prov_code), by = "province") %>%
  
  # Construct location name (city, province, country) for joining with cities
  # reference table
  mutate(loc_full = str_c(city, prov_code, sep = ", "))


# --- Save processed data to CSV ----------------------------------------------
write.csv(
  ccmf_out, 
  "data/ccmf_main.csv", 
  row.names = FALSE, 
  fileEncoding = "UTF-8"
)

write.csv(
  ccmf_categories, 
  "data/ccmf_categories.csv", 
  row.names = FALSE, 
  fileEncoding = "UTF-8"
)

