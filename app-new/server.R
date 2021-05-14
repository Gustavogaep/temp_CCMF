

server <- function(input, output, session){
  
  # --- Get CCMF data -----------------------------------------------------
  
  ccmf <- get_ccmf()
  
  # --- Initial update of user input choices  ---------------------------------

  updateChoices(ccmf, provinces, session)
  
  # --- Define reactive variables ---------------------------------------------
  
  # Process all user input selections to compute a vector of corresponding
  # Incident IDs and an HTML object summarizing the filters applied
  summary_opts <- reactive({
    userSelections(
      ccmf, 
      opts = list(
        year = input$summary_year,
        date_range = input$summary_date_range,
        province = input$summary_province,
        city = input$summary_city,
        incident_category = input$summary_incident_category,
        incident_type = input$summary_incident_type,
        context = input$summary_context,
        ethnic_community = input$summary_ethnic_community,
        identity_based = input$summary_identity_based,
        gender = input$summary_gender
      )
    )
  })
  
  # The reactive vector summary_ids() contains the Incident IDs of all rows
  # matching the selected user inputs, which will be used to filter the data
  # in each plot
  summary_ids <- reactive({
    summary_opts()$ids
  })
  
  # Totals by province
  prov_totals <- reactive({
    provinceTotals(ccmf, summary_ids(), provinces)
  })
  
  # Spatial dataframe with totals by province
  prov_totals_sp <- reactive ({
    provinceSpTotals(prov_totals(), prov_geom)
  })
  
  # Totals by city
  city_totals <- reactive({
    cityTotals(ccmf, summary_ids(), cities_pop)
  })
  
  # Flag for when there is no data for the map, display a message instead of
  # trying to draw the map
  no_map_data <- reactive({
    if (input$summary_province == "N/A") {
      TRUE
    }
    else if (input$summary_map_level == "province") {
      sum(prov_totals() %>% drop_na(prov_code) %>% pull(incidents)) == 0
    }
    else {
      sum(city_totals() %>% pull(incidents)) == 0
    }
  })
  
  # Dataframe for data explorer table
  explorer_df <- reactive({
    dataExplorerDf(ccmf, summary_ids())
  })

  
  # --- Create reactive outputs -----------------------------------------------
  
  # Total number of incidents matching selected user inputs
  output$vbox_total <- renderValueBox({
    totalIncidentsBox(ccmf, summary_ids(), width = 3)
  })

  # Alert box showing which filters are applied
  output$filters_alert <- renderUI({
    summary_opts()$filters_alert
  })
  
  # Line chart of monthly incidents
  output$line_monthly <- renderHighchart({
    timeseriesChart(ccmf, summary_ids(), selected_year = input$summary_year,
                    selected_date_range = input$summary_date_range)
  }) # %>% debounce(5000)
  
  # Leaflet base map
  output$map <- renderLeaflet({
      leafletBaseMap()
  })
  
  # Leaflet map of total incidents or incidents per capita
  observe({
    if (input$summary_map_level == "province") {
      # Choropleth map of province totals
      leafletProxy("map") %>%
        choroplethMap(
          prov_totals_sp(),
          no_data = no_map_data(),
          map_metric = input$summary_map_metric,
          legend = input$summary_map_legend
        )
    }
    else {
      # Bubble map of city totals
      leafletProxy("map") %>%
        bubbleMap(
          city_totals(),
          no_data = no_map_data(),
          map_metric = input$summary_map_metric
          #legend = input$summary_map_legend
        )
    }
    
  })
  
  # Ethnic community - drilldown bar chart
  output$bar_ethnic_community <- renderHighchart({
    ethnicCommunityChart(ccmf, summary_ids())
  })
  
  # Packed bubble plot of ethnic sub-communities
  output$bubble_ethnic_community_sub <- renderHighchart({
    ethnicSubCommunityChart(ccmf, summary_ids())
  })
  
  # Gender bar chart -- percentage of unique incident IDs (no double counting)
  output$bar_gender <- renderHighchart({
    genderChart(ccmf, summary_ids(), include_na = FALSE)
  })
  
  # Identity-based - drilldown bar chart
  output$bar_identity <- renderHighchart({
    identityBasedChart(ccmf, summary_ids())
  })
  
  
  # Incident category - drilldown donut chart
  output$donut_category <- renderHighchart({
    incidentCategoryChart(ccmf, summary_ids())
  })
  
  # Incident type - drilldown bar chart
  output$bar_incident_type <- renderHighchart({
    incidentTypeChart(ccmf, summary_ids())
  })
  
  # # Context - drilldown bar chart
  # output$bar_context <- renderHighchart({
  #   contextChart0(ccmf, summary_ids())
  # })
  
  # Context - drilldown treemap
  output$treemap_context <- renderHighchart({
    contextChart(ccmf, summary_ids())
  })
  
  # Data explorer - table
  output$data_explorer <- renderReactable({
    dataExplorerTable(explorer_df())
  })
  
  # Data explorer - downloader
  output$download <- downloadHandler(
    filename = function(){"ccmf.csv"},
    content = function(fname){
      write.csv(explorer_df(), fname, row.names = FALSE)
    }
  )
  
  
  # --- Sandbox plots ---------------------------------------------------------

  # # Gender donut chart
  # output$donut_gender <- renderHighchart({
  #   genderChartPie(ccmf, summary_ids())
  # })
  
  output$line_monthly2 <- renderHighchart({
    timeseriesChartAlternate(ccmf, summary_ids())
  })
  
  # Ethnic sub-community bar chart
  # ** Limited to top 20 sub-communities **
  output$bar_ethnic_community_sub <-renderHighchart({
    ethnicSubCommunityChart0(ccmf, summary_ids())
  })


  # --- Update UI widgets based on user inputs --------------------------------
  
  # TO DO:
  # - Update choices to pull from the CCMF data, e.g. list of cities, min and
  #   max date range, list of contexts, ethnic communities, etc.
  # - Update dependent choices in widgets, e.g. city choices to reflect the
  #   selected province -- make sure input combinations with no matching
  #   records aren't possible (e.g. "Criminal: Charged" category and province
  #   "Nunavut") or update chart error messages with human readable message
  #   explaining that the chart can't be drawn because there are 0 incidents
  #   for the selected inputs (but probably better to just prevent those
  #   input combinations in the first place).
  # - If additional options are selected in the user inputs, change the title
  #   of the box to e.g. "Additional options selected (n)" where n is the number
  #   of inputs selected in the box
  
  # Update cities to reflect selected province
  observe({
    updateCityChoices(ccmf, session, selected_province = input$summary_province)
  })
  
  
  # Update user input choices based on current selections
  # *** Not working in the desired way -- e.g. selecting a province then removes
  # all the other provinces from the choices in the province input widget
  # observe({
  #   var_names <- c("year", "province", "city", "incident_category", 
  #                  "incident_type", "context", "ethnic_community", 
  #                  "identity_based", "gender")
  #   
  #   for (var_name in var_names) {
  #     
  #     input_name <- paste("summary", var_name, sep = "_")
  #     selected_val <- input[[input_name]]
  #     
  #     # Get list of values for choices
  #     vals <- updateChoiceValues(ccmf, var_name, summary_ids())
  #     
  #     # Update select widget
  #     updateSelectInput(
  #       session, 
  #       input_name, 
  #       choices = vals, 
  #       selected = selected_val
  #     )
  #   }
  # })
  
  # Reset all inputs
  observeEvent(input$summary_reset, {
    updateSelectInput(session, "summary_year", selected = "All")
    updateDateRangeInput(
      session, 
      "summary_date_range", 
      start = ccmf$min_date,
      end = ccmf$max_date
    )
    updateSelectInput(session, "summary_province", selected = "All")
    
    # Multi-select inputs
    for (input_id in c("summary_city", "summary_incident_category",
                       "summary_incident_type", "summary_context",
                       "summary_ethnic_community", "summary_identity_based",
                       "summary_gender")){
      updateSelectInput(session, input_id, selected = "")
    }
    
    # Map inputs
    updateRadioButtons(session, "summary_map_level", selected = "province")
    updateRadioButtons(session, "summary_map_metric", selected = "total")
    updateCheckboxInput(session, "summary_map_legend", value = TRUE)
    
    # Reset Leaflet map view
    opts <- mapOpts()
    leafletProxy("map") %>%
      fitBounds(lng1 = opts$bounds$lng1,
                lat1 = opts$bounds$lat1,
                lng2 = opts$bounds$lng2,
                lat2 = opts$bounds$lat2)
    
  })
  
  # Update label for map summary metric selection
  observe({
    if (input$summary_map_level == "province") {
      updateRadioButtons(session, "summary_map_metric", label = "Shading")
    }
    else if (input$summary_map_level == "city") {
      updateRadioButtons(session, "summary_map_metric", label = "Circle size")
    }
    
  })
  
}

