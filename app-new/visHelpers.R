noDataMessage <- function() {
  "No data to display. Try broadening your data selection."
}

checkData <- function(df_plot, return_df = TRUE) {
  # Write an error message instead of trying to create a chart when the
  # input data has zero rows
  
  validate(
    need(nrow(df_plot) > 0, noDataMessage())
  )
  
  if (return_df) {
    return(df_plot)
  }
}

# hchartStyle <- function() {
#   list(
#     fontFamily = c("Helvetica Neue", "Helvetica", "Arial", "sans-serif")
#   )
# }

hchartCustom <- function(title_text = NULL, ...) {
  
  hc <- hchart(...) %>%
    hc_chart(
      style = list(
        fontFamily = c("Helvetica Neue", "Helvetica", "Arial", "sans-serif")
      )
      
      # # None of these seem to be working.
      # xAxis = list(
      #   title = list(style = list(fontSize = "14px")),
      #   labels = list(style = list(fontSize = "13px"))
      # ),
      # yAxis = list(
      #   title = list(style = list(fontSize = "14px")),
      #   labels = list(style = list(fontSize = "13px"))
      # )
    )
  
  if (!is.null(title_text)) {
    hc <- hc %>% hc_title(
      text = title_text,
      align = "left",
      margin = 30
      # style = list(
      #   color = "#333333",
      #   fontSize = "18px"
      # )
    )
  }
  
  hc$x$hc_opts[["exporting"]] = list(
    enabled = TRUE,
    csv = list(dateFormat = "%B %Y"),
    buttons = list(
      contextButton = list(
        menuItems = c("viewFullscreen", "downloadPNG", "downloadCSV")
        # Omit the "viewData" option for now because it's buggy -- there's no
        # way to hide the table once it's shown
        # menuItems = c("viewFullscreen", "downloadPNG", "downloadCSV", "viewData")
      )
    ),
    chartOptions = list(
      chart = list(
        backgroundColor = "#FFFFFF"
      )
    )
  )
  
  # # Accessibility options don't seem to be working
  # hc$x$hc_opts[["accessibility"]] = list(
  #   enabled = TRUE,
  #   keyboardNavigation = list(enabled = TRUE)
  # )
  
  hc
  
}

# --- Leaflet map helper functions --------------------------------------------

mapOpts <- function() {
  # General options for all maps (choropleth and bubble)
  list(
    bounds = list(lng1 = -125, lat1 = 45, lng2 = -65, lat2 = 70),
    label_options = labelOptions(textsize = "1em"),
    label_col = "label"
  )
}

leafletBaseMap <- function(verbose = TRUE) {
  
  if (verbose) print("*** Drawing base map ***")
  
  opts <- mapOpts()
  
  leaflet () %>%
    addTiles() %>%
    fitBounds(
      lng1 = opts$bounds$lng1, 
      lat1 = opts$bounds$lat1, 
      lng2 = opts$bounds$lng2, 
      lat2 = opts$bounds$lat2
    ) %>%
    addPolygons(
      data = prov_geom,
      fill = FALSE,
      color = "gray",
      weight = 0.5,
      opacity = 0.5,
      group = "Province Outlines"
    ) %>%
    addFullscreenControl() %>%
    addEasyButton(
      easyButton(
        icon = "fa fa-home",
        title = "Reset View",
        onClick = JS(
          "function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"
        )
      )
    ) %>%
    htmlwidgets::onRender(
      JS(
      "
function(el, x){ 
  var map = this; 
  map.whenReady(function(){
    map._initialCenter = map.getCenter(); 
    map._initialZoom = map.getZoom();
  });
}"
    )
  )
}

clearMapData <-function(basemap) {
  basemap %>%
    clearGroup("Selected Data") %>%
    clearControls()
}

choroplethOpts <- function(map_metric = "total") {
  # Options for choropleth maps
  opts <- mapOpts()
  # opts$pal = colorNumeric("Blues", domain = NULL)
  # opts$pal = colorNumeric("YlGnBu", domain = NULL)
  # opts$pal = colorNumeric("GnBu", domain = NULL)
  opts$pal = colorNumeric("PuBu", domain = NULL)
  opts$fill_opacity = 0.9
  if (map_metric == "total") {
    opts$value_col = "incidents"
    opts$legend_title = "Incidents"
  } else {
    opts$value_col = "incidents_per100k"
    opts$legend_title = "Incidents<br>per 100k"
  }
  opts
}

choroplethLegend <- function(map_in, spatial_df, map_metric = "total",
                             legend = TRUE) {
  
  # Add or remove legend for choropleth map
  
  opts <- choroplethOpts(map_metric)
  
  map_out <- map_in %>% clearControls()
  
  if (legend) {
    map_out <- map_out %>%
      addLegend(
        data = spatial_df,
        pal = opts$pal, 
        values = spatial_df[[opts$value_col]],
        title = opts$legend_title,
        group = "Selected Data",
        opacity = opts$fill_opacity
      )
  }
  
  map_out
    
}


choroplethMap <- function(basemap, spatial_df, no_data = FALSE,
                          map_metric = "total", legend = TRUE, verbose = TRUE) {
  
  if (verbose) {
    print("*** Drawing choropleth map ***")
    # print(nrow(data))
    # print(no_data)
    # print(typeof(basemap))
  }
  
  # # Return an error if there is no data for the map
  # validate(
  #   need(!no_data, noDataMessage())
  # )
  
  if (no_data) {
    return(basemap %>% clearMapData())
  }
  
  opts <- choroplethOpts(map_metric)
  
  basemap %>%
    clearMapData() %>%
    addPolygons(
      data = spatial_df,
      fillColor = ~opts$pal(spatial_df[[opts$value_col]]),
      fillOpacity = opts$fill_opacity,
      color = "gray",
      weight = 0.5,
      opacity = 0.5,
      group = "Selected Data",
      label = spatial_df[[opts$label_col]],
      labelOptions = opts$label_options
    ) %>%
    choroplethLegend(
      spatial_df = spatial_df,
      map_metric = map_metric,
      legend = legend
    )
}

bubbleMapOpts <- function(data, map_metric = "total", color_col = "size_group",
                          circle_min_radius = 7, circle_max_radius = 20) {
  # Options for bubble maps
  
  # Note: min and max circle radius are defined in cityTotals() in helpers.R
  opts <- mapOpts()
  opts$circle_min_radius = circle_min_radius
  opts$circle_max_radius = circle_max_radius
  opts$stroke = TRUE
  opts$opacity = 0.8
  opts$weight = 1
  opts$fill_opacity = 0.6
  
  if (is.null(color_col)) {
    opts$pal <- NULL
  } else {
    opts$pal <- colorFactor(
      #topo.colors(n_distinct(data[[color_col]], na.rm = TRUE)),
      #"Dark2",
      #c("#D8456CFF", "#822681FF", "#2D1160FF"),
      # c("#E35932FF", "#9A2865FF", "#450A69FF"),
      c("#D77F37", "#9A2865FF", "#450A69FF"),
      domain = unique(data[[color_col]])
    )
  }
  
  opts$legend_title = "Population"
  if (map_metric == "total") {
    opts$radius_col = "scaled_incidents"
  } else {
    opts$radius_col = "scaled_incidents_per100k"
  }
  opts
}


bubbleMapLegend <- function(map_in, data, map_metric = "total", 
                            color_col = "size_group", legend = TRUE) {
  
  # Add or remove legend for bubble map
  
  opts <- bubbleMapOpts(data, map_metric, color_col)
  
  map_out <- map_in %>% clearControls()
  
  if (legend) {
    map_out <- map_out %>%
      addLegend(
        pal = opts$pal, 
        values = unique(data[[color_col]]),
        title = opts$legend_title,
        group = "Selected Data",
        opacity = opts$fill_opacity
      )
  }
  
  map_out
  
}

bubbleMap <- function(basemap, data, no_data = FALSE, map_metric = "total",
                      color_col = NULL, legend = FALSE, verbose = TRUE) {
  
  if (verbose) {
    print("*** Drawing bubble map ***")
    # print(nrow(data))
    # print(no_data)
    # print(typeof(basemap))
  }
  
  # # Return an error if there is no data for the map
  # validate(
  #   need(!no_data, noDataMessage())
  # )
  
  if (no_data) {
    return(basemap %>% clearMapData())
  }
  
  opts <- bubbleMapOpts(data, map_metric, color_col)
  if (is.null(color_col)) {
    # color <- "#045A8D"
    color <- "#03F"
  } else {
    color <- opts$pal(data[[color_col]])
  }
  
  radius_values <- pmax(
    opts$circle_max_radius * data[[opts$radius_col]],
    opts$circle_min_radius
  )
  
  basemap %>%
    clearMapData() %>%
    addCircleMarkers(
      data$lon,
      data$lat,
      radius = radius_values,
      data = data,
      color = color,
      stroke = opts$stroke,
      weight = opts$weight,
      opacity = opts$opacity,
      fillOpacity = opts$fill_opacity,
      label = data[[opts$label_col]],
      labelOptions = opts$label_options,
      group = "Selected Data"
    ) %>%
    bubbleMapLegend(
      data = data,
      map_metric = map_metric,
      color_col = color_col,
      legend = legend
    )
}

# --- Highcharts drilldown helper functions -----------------------------------

drilldown_setup <- function(ccmf, selected_ids, field_name = "context", top_n = 20, 
                            series_color = NULL, point_colors = NULL, 
                            sort_desc = TRUE, verbose = FALSE) {
  
  # Construct dataframes (main and drilldown) to use in drilldown charts
  
  field_main <- paste(field_name, "_main", sep = "")
  field_sub <- paste(field_name, "_sub", sep = "")
  
  if (verbose) print("*** Starting drilldown setup: df_drilldown***")
  
  df_drilldown <- ccmf[[field_name]] %>%
    filter(incident_id %in% selected_ids) %>%
    rename("category_main" = field_main, "category_sub" = field_sub) %>%
    drop_na(category_sub) %>%
    group_by(category_main, category_sub) %>%
    summarize(count = n_distinct(incident_id)) %>% {
      # Optional: keep only the top n sub-categories within each main category
      if (!is.null(top_n)) 
        slice_max(group_by(., category_main), 
                  order_by = count, 
                  n = top_n,
                  with_ties = FALSE)
      else .
    } %>%
    left_join(
      ccmf$info_ref %>%
        filter(field == field_name) %>%
        drop_na(category_sub) %>%
        select(-c("field", "category_main_abbrev")),
      by = c("category_main", "category_sub")
    ) %>% {
      if (!is.null(point_colors))
        left_join(., point_colors %>% rename("category_main" = "category"))
      else if (!is.null(series_color))
        mutate(., color = series_color)
      else
        mutate(., color = NA)
    } %>%
    mutate(
      category_sub_abbrev = coalesce(category_sub_abbrev, category_sub)
    ) %>% {
      if (sort_desc) arrange(., desc(count))
      else arrange(., count)
    } %>%
    nest(data = c(category_sub, category_sub_abbrev, count, info, color)) %>%
    rename("category" = "category_main") %>%
    mutate(
      id = category,
      data = map(data, rename, "category" = "category_sub", 
                 "category_abbrev" = "category_sub_abbrev")
    )
  
  if (verbose) print("Continuing drilldown setup - df_main")
  
  df_main <- ccmf[[field_name]] %>%
    filter(incident_id %in% selected_ids) %>%
    rename("category" = field_main) %>%
    group_by(category) %>%
    summarize(count = n_distinct(incident_id)) %>%
    left_join(
      ccmf$info_ref %>%
        filter(field == field_name) %>%
        filter(is.na(category_sub)) %>%
        rename("category" = "category_main", "category_abbrev" = "category_main_abbrev") %>%
        select(-c("field", "category_sub", "category_sub_abbrev")),
      by = "category"
    ) %>%
    # Use a left join with df_drilldown so that there is no drilldown link for
    # categories that have no sub-categories
    left_join(
      df_drilldown %>% select(category, id),
      by = "category"
    ) %>%
    mutate(category_abbrev = coalesce(category_abbrev, category)) %>% {
      if (!is.null(point_colors))
        left_join(., point_colors)
      else if (!is.null(series_color))
        mutate(., color = series_color)
      else
        mutate(., color = NA)
    } %>% {
      if (sort_desc) arrange(., desc(count))
      else arrange(., count)
    }
  
  if (verbose) print(df_main %>% select(category, count, color))
  
  # Check custom color specifications, if applicable
  n_color_missing = sum(is.na(df_main$color))

  # If custom color specifications are not used or are incomplete, remove
  # the color column and use default colors
  if (n_color_missing > 0) {
    if (n_color_missing < nrow(df_main)) {
      print("Warning, some custom colours specified but some are missing so reverting to automatic colours")
    }
    if (verbose) {
      print("n_color_missing")
      print(n_color_missing)
    }
    df_main <- df_main %>% select(!color)
    df_drilldown <- df_drilldown %>% 
      mutate(data = map(data, select, !color))
  }
  
  output = list(df_main = df_main, df_drilldown = df_drilldown)
  
  return(output)
}


data_to_hierarchical_drilldown <- function(df_main) {
  
  # Additional data transformation for treemap drilldown
  
  treemap_input <- df_main %>%
    data_to_hierarchical(category, count)
  
  for (i in 1:length(treemap_input)) {
    # Get additional data from dataframe and add to the list object
    df_row <- df_main %>% 
      filter(category == treemap_input[[i]][["name"]]) %>%
      # There should never be more than one match, but limit it to 1 just in case
      head(1) 
    
    treemap_input[[i]][["drilldown"]] <- df_row %>% pull(id)
    treemap_input[[i]][["info"]] <- df_row %>% pull(info)
  }
  
  return(treemap_input)
  
}


drilldown_chart <- function(
  chart_data, 
  chart_type = "bar",
  title_text = NULL,
  xaxis_title = list(text = ""),
  yaxis_title = list(text = "Incidents"),
  tooltip_format = NULL) {
  
  # Create a Highcharts drilldown chart (bar, column, or pie)
  
  # Make sure there is data to plot
  checkData(chart_data$df_main, return_df = FALSE)
  
  # Set chart aesthetics variables
  if ("color" %in% colnames(chart_data$df_main)) {
    # Use custom colors
    chart_hcaes <- hcaes(x = category, y = count, color = color, drilldown = id)
  } else {
    # Use default colors
    chart_hcaes <- hcaes(x = category, y = count, drilldown = id)
  }
  
  # Tooltip format
  if (is.null(tooltip_format)) {
    if (chart_type == "pie") {
      tooltip_format <- '<span style="color:{point.color}">\u25CF</span><strong> {point.name}</strong><br>{point.info}<br>{point.y} incidents<br>{point.percentage:.1f}%'
    }
    else if (chart_type == "treemap") {
      tooltip_format <- '<strong>{point.name}</strong><br>{point.info}<br>
  <span style="color:{point.color}">\u25CF</span> Incidents: {point.value}'
    }
    else {
      tooltip_format <- '<strong>{point.name}</strong><br>{point.info}<br>
  <span style="color:{point.color}">\u25CF</span> Incidents: {point.y}'
    }
  }
  
  # Drill up button options
  if (chart_type %in% c("bar", "pie")) {
    drillup_button <- list(
      position = list("verticalAlign" = "bottom", y = -50)
    )
  } else {
    drillup_button <- list(
      position = list("verticalAlign" = "top")
    )
  }
  
  # Transform data and initialize chart
  if (chart_type == "treemap") {
    
    chart_data$df_drilldown <- chart_data$df_drilldown %>%
      mutate(type = chart_type) %>%
      mutate(data = map(data, data_to_hierarchical, category, count))
    
    hc <- chart_data$df_main %>%
      data_to_hierarchical_drilldown() %>%
      hchartCustom(title_text = title_text, type = "treemap")
    
  } else {
    chart_data$df_drilldown <- chart_data$df_drilldown %>%
      mutate(type = chart_type) %>%
      mutate(data = map(data, mutate, name = category, y = count)) %>%
      mutate(data = map(data, list_parse))
    
    hc <- chart_data$df_main %>%
      hchartCustom(title_text = title_text, chart_type, chart_hcaes)
  }
  
  # Add drilldown data and customize chart
  hc <- hc %>% 
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = list_parse(chart_data$df_drilldown),
      drillUpButton = drillup_button
    ) %>%
    hc_tooltip(
      headerFormat = "",
      pointFormat = tooltip_format
    ) %>%
    hc_xAxis(title = xaxis_title) %>%
    hc_yAxis(title = yaxis_title) %>%
    hc_plotOptions(
      bar = list(
        dataLabels = list(
          enabled = TRUE, 
          format = "{point.y}"
        )
      ),
      column = list(
        dataLabels = list(
          enabled = TRUE, 
          format = "{point.y}"
        )
      ),
      pie = list(
        innerSize = "30%",
        dataLabels = list(
          enabled = TRUE, 
          distance = 5,
          format =  "{point.name}<br>{point.percentage:.1f}%"
        )
      ),
      treemap = list(
        layoutAlgorithm = "squarified",
        layoutStartingDirection = "horizontal",
        dataLabels = list(
          enabled = TRUE, 
          format =  "{point.name}<br>{point.value}"
        )
      )
    )
  
  return(hc)
}
