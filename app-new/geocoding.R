# Get coordinates of each city in the dataset. This takes a few minutes and
# should be done in a separate process from the dashboard app, e.g. during
# scheduled download of Google Sheet data to local server. Should also modify
# so that it only calls the geocoding function for new cities, and uses the
# previously obtained coordinates for any cities that have already been
# geocoded.

library(tmaptools)

source("global.R")

# Get listing of all unique cities in the data
cities <- ccmf$main %>%
  drop_na(city_or_region) %>%
  group_by(city_or_region, prov_code, loc_full) %>% 
  summarize(count = n()) %>%
  select(!count)

# Get coordinates for each city
coords <- geocode_OSM(
  str_c(cities$loc_full, "Canada", sep = ", "), 
  as.data.frame = TRUE, 
  keep.unfound = TRUE
) %>%
  mutate(loc_full = str_replace_all(query, ", Canada", "")) %>%
  select(loc_full, lat, lon)

# Join coordinates with main table
cities <- cities %>%
  left_join(coords, by = "loc_full")

# Save to CSV
write.csv(cities, "data/cities.csv", row.names = FALSE, na = "")