totalIncidentsBox <- function(ccmf, selected_ids, width = 3){
  valueBox(
    h1(
      ccmf$main %>%
        filter(incident_id %in% selected_ids) %>%
        nrow()
    ),
    "Incidents",
    width = width
  )
}

timeseriesChart <- function(ccmf, selected_ids, selected_year, 
                            selected_date_range,
                            title_text = "Trends over time", verbose = TRUE) {
  
  if (verbose) print("*** Drawing time series chart ***")
  
  # Minimum and maximum dates for x-axis
  date_range <- dateRange(ccmf, selected_ids, selected_year, selected_date_range)
  
  # Index of evenly spaced dates
  monthly_index <- data.frame(
    year_month = seq(
      floor_date(date_range[1], unit = "month"), 
      floor_date(date_range[2], unit = "month"), 
      by = "months"
    )
  )
  
  df_plot <- ccmf$main %>%
    filter(incident_id %in% selected_ids) %>%
    mutate(year_month = floor_date(incident_date, unit = "month")) %>%
    group_by(year_month) %>%
    summarize(count = n()) %>%
    full_join(monthly_index, by = "year_month") %>%
    replace_na(list(count = 0)) %>%
    arrange(year_month)
  
  checkData(df_plot %>% filter(count > 0), return_df = FALSE)
  
  df_plot %>%
    hchartCustom(
      title_text = title_text,
      "line",
      hcaes(x = year_month, y = count),
      name = "Incidents",
      color = "#5f98cf"
      #color = "#BF1722"
      #color = "#081D58"
    ) %>%
    hc_xAxis(
      title = list(text = ""),
      dateTimeLabelFormats = list(
        month = "%b %Y",
        
        # The data is monthly resolution, so only show month-year when less
        # than a month is selected for the timeseries chart
        week = "%b %Y",
        day = "%b %Y"
      )
    ) %>%
    hc_yAxis(title = list(text = "Incidents per month"))
}

ethnicCommunityChart <- function(ccmf, selected_ids, series_color = "#f7a35c",
                                 chart_type = "bar", 
                                 title_text = "Ethnic communities", verbose = TRUE) {
  
  if (verbose) print("*** Drawing ethnic community chart ***")
  
  drilldown_setup(
    ccmf,
    selected_ids,
    field_name = "ethnic_community",
    series_color = series_color
  ) %>%
    drilldown_chart(chart_type = chart_type, title_text = title_text)
}

ethnicSubCommunityChart <- function(ccmf, selected_ids, 
                                    title_text = "Comparison between sub-ethnic communities", verbose = TRUE) {
  
  if (verbose) print("*** Drawing ethnic sub-community chart ***")
  
  ccmf$ethnic_community %>%
    filter(incident_id %in% selected_ids) %>%
    drop_na(ethnic_community_sub) %>%
    group_by(ethnic_community_main, ethnic_community_sub) %>%
    summarize(count = n()) %>%
    # left_join(
    #   ccmf$info_ref %>%
    #     filter(field == "ethnic_community") %>%
    #     drop_na(category_sub) %>%
    #     rename("ethnic_community_main" = "category_main",
    #            "ethnic_community_sub" = "category_sub") %>%
    #     select(ethnic_community_main, ethnic_community_sub, info)
    # ) %>%
    checkData() %>%
    hchartCustom(
      title_text = title_text,
      "packedbubble",
      hcaes(name = ethnic_community_sub, value = count, group = ethnic_community_main),
    ) %>% 
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = "<b>{point.name}:</b> {point.value}"
    ) %>% 
    hc_plotOptions(
      packedbubble = list(
        minSize = "30%",
        maxSize = "120%",
        layoutAlgorithm = list(
          gravitationalConstant =  0.001,
          splitSeries =  FALSE, # TRUE to group points
          enableSimulation = FALSE,
          bubblePadding = 10
        ),
        dataLabels = list(
          enabled = TRUE,
          format = "{point.name}",
          style = list(
            color = "black",
            textOutline = "none",
            fontWeight = "normal"
          )
        )
      )
    )
}

genderChart <- function(ccmf, selected_ids, include_na, title_text = "Gender") {
  ccmf$gender %>%
    filter(incident_id %in% selected_ids) %>% {
      if (!include_na) filter(., gender != "N/A")
      else .
    } %>%
    mutate(percent = 100 / n_distinct(incident_id)) %>%
    group_by(gender) %>%
    summarize(
      count = n(),
      percent = sum(percent)
    ) %>%
    checkData() %>%
    hchartCustom(
      title_text = title_text,
      "column",
      hcaes(x = gender, y = percent),
      color = "#2b908f",
      dataLabels = list(
        enabled = TRUE,
        format = "{point.percent:.1f}%"
      )
    ) %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_yAxis(
      title = list(text = "% of Incidents"),
      labels = list(format = "{value}%")
    ) %>%
    hc_tooltip(
      pointFormat = '<span style="color:{point.color}">\u25CF</span><strong> {point.name}</strong><br>{point.count} incidents<br>{point.y:.1f}%',
      headerFormat = ""
    )
}

identityBasedChart <- function(ccmf, selected_ids, series_color = "#8085e9",
                               chart_type = "bar",
                               title_text = "Identity-based intersectionalities",
                               verbose = TRUE) {
  
  if (verbose) print("*** Drawing identity-based chart ***")
  
  drilldown_setup(
    ccmf, 
    selected_ids, 
    field_name = "identity_based", 
    series_color = series_color
  ) %>%
    drilldown_chart(chart_type = chart_type, title_text = title_text)
}


incidentCategoryChart <- function(ccmf, selected_ids, 
                                  title_text = "Criminal vs. non-criminal",
                                  verbose = TRUE) {
  
  if (verbose) print("*** Drawing incident category chart ***")
  
  drilldown_setup(
    ccmf, 
    selected_ids, 
    field_name = "incident_category", 
    sort_desc = FALSE
  ) %>%
    drilldown_chart(chart_type = "pie", title_text = title_text)
}

incidentTypeChart <- function(ccmf, selected_ids, series_color = "#f15c80", 
                              chart_type = "bar", 
                              title_text = "Type of incident",
                              verbose = TRUE) {
  
  if (verbose) print("*** Drawing incident type chart ***")
  
  drilldown_setup(
    ccmf, 
    selected_ids, 
    field_name = "incident_type", 
    series_color = series_color
  ) %>%
    drilldown_chart(chart_type = chart_type, title_text = title_text)
}

# contextChart0 <- function(ccmf, selected_ids, series_color = "#2b908f",
#                           chart_type = "bar",
#                           title_text = "Incident occurred in the context of:") {
#   drilldown_setup(
#     ccmf, 
#     selected_ids, 
#     field_name = "context", 
#     series_color = series_color
#   ) %>%
#     drilldown_chart(chart_type = chart_type, title_text = title_text)
# }

contextChart <- function(ccmf, selected_ids, chart_type = "treemap",
                         title_text = "Incident occurred in the context of:",
                         verbose = TRUE) {
  
  if (verbose) print("*** Drawing context chart ***")
  
  drilldown_setup(
    ccmf, 
    selected_ids, 
    field_name = "context2" 
  ) %>%
    drilldown_chart(chart_type = chart_type, title_text = title_text)
}

dataExplorerTable <- function(df_explorer, verbose = TRUE) {
  
  if (verbose) print("*** Creating data explorer table ***")
  
  df_explorer %>%
    mutate(URL = paste0("<a href='", URL,"' target='_blank'>", icon("link"),"</a>")) %>%
    reactable(
      sortable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      resizable = TRUE,
      highlight = TRUE,
      compact = TRUE,
      defaultPageSize = 5,showSortable = TRUE,
      columns = list(
        URL = colDef(html = TRUE),
        Description = colDef(width = 200)
      )
    )
}

# --- Sandbox charts -----------------------------------------------------------

# genderChartPie <- function(ccmf, selected_ids, title_text = "Gender",
#                            verbose = TRUE) {
#   
#   if (verbose) print("*** Drawing gender chart ***")
#   
#   ccmf$gender %>%
#     filter(gender != "N/A") %>%
#     filter(incident_id %in% selected_ids) %>%
#     group_by(gender) %>%
#     summarize(count = n()) %>%
#     left_join(data.frame(gender = c("Female", "Male", "Other/Unknown"),
#                          color = c("#f7a35c", "#2b908f", "#f15c80"))) %>%
#     arrange(gender) %>%
#     checkData() %>%
#     hchartCustom(
#       title_text = title_text,
#       "pie",
#       hcaes(name = gender, y = count, color = color),
#       name = "Incidents",
#       dataLabels = list(enabled = TRUE, 
#                         distance = 5,
#                         format =  "{point.name}<br>{point.percentage:.1f}%"),
#       innerSize = "30%"
#     ) %>%
#     hc_tooltip(
#       pointFormat = '<span style="color:{point.color}">\u25CF</span><strong> {point.name}</strong><br>{point.y} incidents<br>{point.percentage:.1f}%',
#       headerFormat = ""
#     )
# }

ethnicSubCommunityChart0 <- function(ccmf, selected_ids) {
  # Ethnic sub-community bar chart
  # ** Limited to top 20 sub-communities **
  tt_sub_eth <- '{point.ethnic_community_main}: {point.name}<br>
  <span style="color:{point.color}">\u25CF</span> Incidents: {point.y}'
  
  ccmf$ethnic_community %>%
    filter(incident_id %in% selected_ids) %>%
    drop_na(ethnic_community_sub) %>%
    group_by(ethnic_community_main, ethnic_community_sub) %>%
    summarize(count = n()) %>%
    # left_join(
    #   ccmf$info_ref %>%
    #     filter(field == "ethnic_community") %>%
    #     drop_na(category_sub) %>%
    #     rename("ethnic_community_main" = "category_main",
    #            "ethnic_community_sub" = "category_sub") %>%
    #     select(ethnic_community_main, ethnic_community_sub, info)
    # ) %>%
    arrange(desc(count)) %>%
    head(20) %>%
    checkData() %>%
    hchart(
      "bar",
      hcaes(x = ethnic_community_sub, y = count, color = ethnic_community_main),
      name = "Incidents",
      dataLabels = list(enabled = TRUE)
    ) %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_yAxis(title = list(text = "Incidents")) %>%
    hc_tooltip(
      pointFormat = tt_sub_eth,
      headerFormat = ""
    )
}



# output$bubble_gender <- renderHighchart({
#   ccmf$gender %>%
#     filter(incident_id %in% summary_ids()) %>%
#     mutate(percent = 100 / n_distinct(incident_id)) %>%
#     group_by(gender) %>%
#     summarize(
#       count = n(),
#       percent = sum(percent)
#     ) %>%
#     left_join(data.frame(gender = c("Female", "Male", "Other/Unknown"),
#                          color = c("#f7a35c", "#2b908f", "#f15c80"))) %>%
#     hchart(
#       "packedbubble",
#       hcaes(name = gender, value = percent, color = color),
#       dataLabels = list(
#         enabled = TRUE,
#         format = "{point.name}<br>{point.value:.1f}%"
#       )
#     ) %>%
#     hc_plotOptions(
#       packedbubble = list(
#         # minSize = "30%",
#         # maxSize = "200%",
#         layoutAlgorithm = list(
#           gravitationalConstant =  0.001,
#           splitSeries =  FALSE, # TRUE to group points
#           enableSimulation = FALSE,
#           bubblePadding = 10
#         )
#       )
#     )
# })

timeseriesChartAlternate <- function(ccmf, selected_ids,
                                     title_text = "Trends over time") {
  # TO DO: Add Totals group, make sure each group is indexed properly with
  # monthly_index()
  
  df_plot <- ccmf$main %>%
    filter(incident_id %in% selected_ids) %>%
    mutate(
      year_month = floor_date(incident_date, unit = "month"),
      incident_category = str_replace(incident_category,":.*", "" )
    ) %>%
    group_by(incident_category, year_month) %>%
    summarize(count = n()) %>%
    arrange(incident_category, year_month)
  
  checkData(df_plot %>% filter(count > 0), return_df = FALSE)
  
  df_plot %>%
    hchartCustom(
      title_text = title_text,
      "area",
      hcaes(x = year_month, y = count, group = incident_category),
      stacking = "normal"
    ) %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_yAxis(title = list(text = "Incidents per month")) %>%
    hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE
    )
}