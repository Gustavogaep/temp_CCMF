

library(dplyr)
library(echarts4r)
library(echarts4r.maps)



make_map <- function(data_map) {
  data_map %>% 
    e_charts(Province) %>%
    em_map("Canada") %>% 
    e_map(Count, map = "Canada") %>% 
    e_visual_map(Count) %>% 
    e_title("Racise Crimes in Canada", "Sorted by province") %>% 
    e_tooltip()
}


