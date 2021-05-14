

# required libraries ------------------------------------------------------
library(shiny)
#library(shinydashboard)
library(bs4Dash)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
# library(ggplot2)
# library(echarts4r)
# library(echarts4r.maps)
library(shinyWidgets)
library(highcharter)
library(reactable)
#library(fontawesome)
#library(tablerDash)
library(sf)
#library(tmap)
library(leaflet)
library(DT)
library(lubridate)
library(googlesheets4)


# the data ----------------------------------------------------------------

# access google sheet as an anonymous user
gs4_deauth()

# google sheet containing the data
ss_url <- "https://docs.google.com/spreadsheets/d/11jB7OgjugRAOJTVpF1Kw9mQyCwZtsBJGOcAMSOmwtWY/edit?usp=sharing"

# main data table
data <- read_sheet(
  ss_url,
  sheet = "Data",
  col_types = "i-ccDiiicccccc--ccccc---"
) %>%
  # construct incident date from year-month-day components, filling any
  # missing days with 1
  mutate(`Incident Date` = paste(`Incident Year-Month`, 
                                 sprintf("%02d", replace_na(`Incident Day of Month`, 1)), 
                                 sep="-")) %>%
  mutate(`Incident Date` = as_date(`Incident Date`))


# ethnic community <=> sub-community lookup table
ethnicity_lookup <- read_sheet(ss_url, "Ethnicities", col_types="cc")


# required for the value boxes --------------------------------------------
data_for_value_boxes <- data %>% 
  group_by(`City or Region`) %>% 
  summarise(n1 = n()) %>% 
  filter(`City or Region` != "NA") %>% 
  arrange(desc(n1)) %>% 
  slice(1:4)

## counts for each province, used for the map ------------------------------
data_map <- data %>% 
  group_by(Province) %>% 
  summarise(Count = n())

data_map_shape <- read_sf("./data/shape_file/canada.shp") %>% 
  left_join(data_map, by = c("NAME_1" = "Province")) %>% 
  select(NAME_1, Count)

labels <-
  paste0("<b>", data_map_shape$NAME_1, "</b>:<br>", data_map_shape$Count) %>% 
  map(~ htmltools::HTML(.))

pal <- colorBin(
  "Reds",
  na.color = "#FFFFFF",
  domain = data_map_shape$Count,
  bins = 5
)

# data for gender plot & table(#3) ----------------------------------------
## cleaning the gender values
dt_gender <- data %>%
  mutate(`Gender of Victim(s)` = recode(`Gender of Victim(s)`, "Female, Male" = "Both"),
         `Gender of Victim(s)` = ifelse(is.na(`Gender of Victim(s)`), "Missing", `Gender of Victim(s)`))

## function that takes in grouping variables and returns the data.
## can input both dt_gender & dt_type in this function
make_ready_data <- function(dt, g1, g2) {
  dt %>% 
    group_by({{g1}}, {{g2}}) %>% 
    summarise(n = n()) %>% 
    ungroup()
}


## gender by province, will be joined with type by province
dt_gender_table <- dt_gender %>% 
  make_ready_data(g1 = Province, g2 = `Gender of Victim(s)`)

# data for type & table(#3) -----------------------------------------------
## cleaning the data
dt_type <- data %>% 
  mutate(Type = ifelse(str_detect(`Type of Incident`, ","),
                       str_remove(`Type of Incident`, ": .+(?=,)"),
                       str_remove(`Type of Incident`, ": .+"))) %>% 
  mutate(Type = ifelse(str_detect(Type, ","),
                       str_remove(Type, ": .+"),
                       Type)) %>% 
  separate(Type, into = c("Type1", "Type2"), sep = ", ") %>% 
  mutate(Type = purrr::map2(Type1, Type2, function(x,y){
    na.omit(c(x, y))
  })) %>% 
  unnest(cols = c(Type))

dt_type_table <- dt_type %>% 
  make_ready_data(g1 = Province, g2 = Type)

joined <- dt_type_table %>% 
  left_join(dt_gender_table, by = "Province")

# 
#   group_by(Province, Type) %>% 
#   summarise(n = n()) %>% 
#   ungroup()

# data for count by date bar ---------------------------------------------
dt_dates_count_bar <- data %>% 
  make_ready_data(g1 = `Incident Date`, g2 = NULL) %>% 
  arrange(desc(n)) %>% 
  slice(1:5) %>% 
  mutate(`Incident Date` = as.character(`Incident Date`))


# data for identity ------------------------------------------------------
## cleaning the data

dt_identity <- data %>% 
  mutate(iden_type = str_remove_all(`Identity-Based`, "Religious discrimination: ")) %>% 
  separate(iden_type, into = c("iden_1", "iden_2", "iden_3", "iden_4"), sep = ", ") %>% 
  mutate(iden_type1 = map2(iden_1, iden_2, function(x,y){
    na.omit(c(x,y))
  })) %>% 
  mutate(iden_type2 = map2(iden_3, iden_4, function(x,y){
    na.omit(c(x,y))
  })) %>% 
  mutate(iden_type = map2(iden_type1, iden_type2, function(x,y){
    na.omit(c(x,y))
  })) %>% 
  unnest(cols = c(iden_type))

dt_identity_based_bar <- dt_identity %>% 
  make_ready_data(g1 = iden_type, g2 = NULL) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  rename(`Identity-Based` = iden_type)

dt_identity_based_line <- dt_identity %>% 
  mutate(Year = year(`Incident Date`)) %>% 
  make_ready_data(g1 = Year, g2 = iden_type) %>% 
  rename(`Identity-Based` = iden_type)

# data for context ----------------------------------------------------
## cleaning the data
dt_context <- data %>% 
  mutate(con_type = ifelse(str_detect(Context, ","),
                           str_remove(Context, ": .+(?=,)"),
                           str_remove(Context, ": .+"))) %>% 
  mutate(con_type = ifelse(str_detect(con_type, ","),
                           str_remove(con_type, ": .+"),
                           con_type)) %>% 
  separate(con_type, into = c("con_type1", "con_type2"), sep = ", ") %>% 
  mutate(con_type = purrr::map2(con_type1, con_type2, function(x,y){
    na.omit(c(x, y))
  })) %>% 
  unnest(cols = c(con_type))

dt_context_bar <- dt_context %>% 
  make_ready_data(g1 = con_type, g2 = NULL) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  rename(Context = con_type)

dt_context_line <- dt_context %>% 
  mutate(Year = year(`Incident Date`)) %>% 
  make_ready_data(g1 = Year, g2 = con_type) %>% 
  rename(Context = con_type)

# data for yearly count line
dt_yearly_count_line <- data %>% 
  mutate(Year = year(`Incident Date`)) %>% 
  make_ready_data(g1 = Year, g2 = NULL) %>% 
  filter(!is.na(Year)) %>% 
  mutate(Year = as.character(Year))

# data for sunburst
dt_sunburst <- data %>% 
  filter(!is.na(Province) | !is.na(`City or Region`)) %>% 
  group_by(Province, `City or Region`) %>% 
  summarise(n = n()) %>% 
  data_to_hierarchical(c(Province, `City or Region`), n)

# data for community gender bar
dt_community_gender <- data %>% 
  separate(`Ethnic Community`, into = c("com_type1", "com_type2"), sep = ", ") %>% 
  mutate(community = purrr::map2(com_type1, com_type2, function(x,y){
    na.omit(c(x, y))
  })) %>% 
  unnest(cols = c(community)) %>% 
  filter(community != "N-A" & !is.na(`Gender of Victim(s)`)) %>% 
  make_ready_data(g1 = community, g2 = `Gender of Victim(s)`) %>% 
  arrange(desc(n))

#data for month bar
dt_month <- data %>% 
  filter(!is.na(`Incident Date`)) %>% 
  mutate(Month = factor(month.name[month(`Incident Date`)],
                        levels = month.name,
                        ordered = TRUE)) %>% 
  make_ready_data(Month) %>% 
  mutate(col = ifelse(n %in% head(sort(.$n, decreasing = TRUE), 3),
                      "#9a8f61","#f6edc9"))