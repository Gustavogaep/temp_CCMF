# --- Libraries ---------------------------------------------------------------

# Shiny
library(shiny)
library(bs4Dash)
library(htmlwidgets)

# Data cleaning
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)

# Geospatial
library(leaflet)
library(leaflet.extras)
library(rgdal)

# Visualization
library(highcharter)
library(reactable)

# Helper functions
source("dataHelpers.R")
source("uiElements.R")
source("serverHelpers.R")
source("visHelpers.R")
source("visElements.R")

# Highcharter configuration
lang <- getOption("highcharter.lang")
lang$drillUpText <- "<< Back"
options(highcharter.lang = lang)


# --- Get reference data -------------------------------------------------------

# Provinces references table: province name, 2-letter code, population
# TO DO: Use consolidated Census 2016 file (population centres + provinces)
# instead of `pop_prov_2016.csv`
provinces <- read.csv("./data/ref/pop_prov_2016.csv")

# City populations
# -- Size groups for binning population
breaks <- c(0, 9999, 99999, 1e9)
labels <- c("< 10,000", "10,000-99,999", "100,000+")

# -- Read data and bin into size groups
cities_pop <- read.csv("./data/ref/census_2016.csv", encoding = "UTF-8") %>%
  filter(type == "Population centre") %>%
  select(name, province, prov_code, location_label, population_2016) %>%
  rename("loc_full" = "location_label", "population" = "population_2016") %>%
  mutate(
    size_group = cut(population, breaks = breaks, labels = labels, 
                     ordered_result = TRUE)
  )

# Province boundaries for map
if (!file.exists("./data/ref/ne_50m_admin_1_states_provinces_lakes/ne_50m_admin_1_states_provinces_lakes.dbf")){
  download.file(file.path('http://www.naturalearthdata.com/http/',
                          'www.naturalearthdata.com/download/50m/cultural',
                          'ne_50m_admin_1_states_provinces_lakes.zip'), 
                f <- tempfile())
  unzip(f, exdir = "./data/ref/ne_50m_admin_1_states_provinces_lakes")
  rm(f)
}

prov_geom <- readOGR("./data/ref/ne_50m_admin_1_states_provinces_lakes", 
                     'ne_50m_admin_1_states_provinces_lakes', 
                     encoding='UTF-8')

# Extract Canadian provinces only
prov_geom <- subset(prov_geom, iso_a2 == "CA")
