



server <- function(input, output, session){
  
  
  updateSelectInput(
    session = session,
    inputId = "province",
    choices = data$Province
  )
  
  output$table_3_1 <- renderReactable({
    dt_type_table %>%
      filter(Province == input$province) %>% 
      rename("No. of Incidents" = n, "Type of Incident" = Type) %>%
      select(-Province) %>% 
      reactable(bordered = TRUE)
  })
  
  output$table_3_2 <- renderReactable({
    dt_gender_table %>%
      filter(Province == input$province) %>% 
      rename("No. of Incidents" = n) %>% 
      select(-Province) %>% 
      reactable(bordered = TRUE)
      
  })
  
  # output$dates_count_line <- renderPlot({
  #   data %>% 
  #     group_by(`Incident Date`) %>% 
  #     summarise(n = n()) %>% 
  #     ggplot(aes(`Incident Date`, n)) +
  #     geom_path(group = 1) +
  #     geom_point() +
  #     labs(y = "No. of Incidents", title = "All the Dates with Number of Incidents") +
  #     theme_minimal()
  # })
  
  output$dates_count_bar <- renderHighchart(({
    dt_dates_count_bar %>% 
      hchart('bar',
             hcaes(x = `Incident Date`, y = n),
             color = "#c9e9f6",
             borderColor = "black",
             name = "No. of Incidents",
             dataLabels = list(enabled = TRUE)) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "No. of Incidents"))
  }))
  
  # output$map <- renderPlot({
  #   ggplot() +
  #     geom_sf(data = data_map, aes(fill = Count), col = "black") +
  #     scale_color_brewer(type = "seq", palette = "Reds") +
  #     labs(title = "Number of Crimes in Canada") +
  #     theme_minimal()
  # })
  
  # output$map <- renderEcharts4r({
  #   make_map(data_map)
  # })
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("Esri.NatGeoWorldMap", group = "Esri.NatGeoWorldMap") %>%
      addProviderTiles('Esri.WorldTerrain', group = "Esri.WorldTerrain") %>%
      addProviderTiles("Stamen.Toner", group = "Toner by Stamen") %>%
      addPolygons(
        data = data_map_shape,
        stroke = TRUE, weight = 1, color = "black", opacity = 1,
        fillColor = ~pal(Count),
        popupOptions = popupOptions(maxWidth = 500, maxHeight = 200),
        fillOpacity = 0.8, smoothFactor = 0.5,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLayersControl(
        baseGroups = c("Esri.NatGeoWorldMap", "Esri.WorldTerrain", "Toner by Stamen")
      ) %>%
      addLegend(
        position = 'topright',
        pal = pal, values = data_map_shape$Count,
        labFormat = function(type, cuts, p) {
          n = length(cuts)
          paste0(cuts[-n], " &ndash; ", cuts[-1])
        },
        title = 'Count',
        na.label = 'No Data',
        opacity = .8
      )
    
    # tmap_canada <- data_map_shape %>% 
    #   tm_shape() +
    #   tm_fill(col = "Count", palette = "Reds") +
    #   tm_borders() +
    #   tm_style("natural")
    # tmap_leaflet(tmap_canada, mode = "view", show = TRUE, add.titles = TRUE) %>% 
    #   setView(-100, 72,  zoom = 3)
  })
  
  output$topcities_v1 <- renderInfoBox({
    d1 <- data_for_value_boxes
    #valueBox(value = , subtitle = , width = 3, status = "danger")
    infoBox(
      tabName = "#1",
      title = d1[1, 1],
      value = paste0("Incidents: ", d1[1, 2]),
      status = "warning",
      icon = "exclamation-circle"
    )
  })
  
  output$topcities_v2 <- renderInfoBox({
    d1 <- data_for_value_boxes
    #valueBox(value = , subtitle = , width = 3, status = "danger")
    infoBox(
      tabName = "#2",
      title = d1[2, 1],
      value = paste0("Incidents: ", d1[2, 2]),
      status = "warning",
      icon = "exclamation-circle",class = "tt"
    )
  })
  
  output$topcities_v3 <- renderInfoBox({
    d1 <- data_for_value_boxes
    #valueBox(value = , subtitle = , width = 3, status = "danger")
    infoBox(
      tabName = "#3",
      title = d1[3, 1],
      value = paste0("Incidents: ", d1[3, 2]),
      status = "warning",
      icon = "exclamation-circle",
    )
  })
  
  output$topcities_v4 <- renderInfoBox({
    d1 <- data_for_value_boxes
    #valueBox(value = , subtitle = , width = 3, status = "danger")
    infoBox(
      tabName = "#4",
      title = d1[4, 1],
      value = paste0("Incidents: ", d1[4, 2]),
      status = "warning",
      icon = "exclamation-circle",
    )
  })
  
  # output$yearly_count <- renderPlot({
  #   dt_yearlycountline %>% 
  #     hchart('line',
  #            hcaes(x = Year, y = n)) %>% 
  #     hc_xAxis(title = list(text = "")) %>% 
  #     hc_yAxis(title = list(text = "No. of Incidents"))
  # })
  
  output$identity_based_bar <- renderHighchart({
    dt_identity_based_bar %>% 
      hchart('bar',
             hcaes(x = `Identity-Based`, y = n),
             color = "#f6edc9",
             borderColor = "black",
             name = "No. of Incidents",
             dataLabels = list(enabled = TRUE)) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "No. of Incidents"))
  })
  
  output$identity_based_line <- renderHighchart({
    dt_identity_based_line %>% 
      hchart('line',
             hcaes(
               x = Year,
               y = n,
               group = `Identity-Based`)) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "No. of Incidents"))
  })
  
  output$context_bar <- renderHighchart({
    dt_context_bar %>% 
      hchart('bar',
             hcaes(x = Context, y = n),
             color = "#d8d8d8",
             borderColor = "black",
             name = "No. of Incidents",
             dataLabels = list(enabled = TRUE)) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "No. of Incidents"))
  })
  
  output$context_line <- renderHighchart({
    dt_context_line %>% 
      hchart('line',
             hcaes(
               x = Year,
               y = n,
               group = Context
             )) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "No. of Incidents"))
  })
  
  # output$type_bar <- renderHighchart({
  #   dt_type %>% 
  #     make_ready_data(g1 = Type, g2 = NULL) %>% 
  #     arrange(desc(n)) %>% 
  #     hchart('bar',
  #            hcaes(x = Type, y = n),
  #            color = "red",
  #            borderColor = "black",
  #            name = "No. of Incidents") %>% 
  #     hc_xAxis(title = list(text = "")) %>% 
  #     hc_yAxis(title = list(text = "No. of Incidents"))
  # })
  
  # output$gender_line <- renderHighchart({
  #   dt_gender %>% 
  #     mutate(Year = year(`Incident Date`)) %>% 
  #     make_ready_data(g1 = Year, g2 = `Gender of Victim(s)`) %>%
  #     hchart('line', 
  #            hcaes(
  #              x = Year,
  #              y = n,
  #              group = `Gender of Victim(s)`)) %>% 
  #     hc_xAxis(title = list(text = "")) %>% 
  #     hc_yAxis(title = list(text = "No. of Incidents"))
  # })
  
  # output$gender_bar <- renderHighchart({
  #   make_ready_data(dt_gender, g1 = `Gender of Victim(s)`, g2 = NULL) %>% 
  #     arrange(desc(n)) %>% 
  #     hchart('bar',
  #            hcaes(x = `Gender of Victim(s)`, y = n),
  #            color = "steelblue",
  #            borderColor = "black",
  #            name = "No. of Incidents") %>% 
  #     hc_xAxis(title = list(text = "")) %>% 
  #     hc_yAxis(title = list(text = "No. of Incidents"))
  # })
  
  output$communityline <- renderHighchart({
    data %>% 
      separate(`Ethnic Community`, sep = ", ", into = c("community_1", "community_2", "community_3")) %>% 
      mutate(community = map2(community_1, community_2, function(x,y){
        na.omit(c(x,y))
      })) %>% 
      mutate(community = map2(community, community_3, function(x,y){
        na.omit(c(x,y))
      })) %>% 
      unnest(cols = c(community)) %>% 
      mutate(community = str_to_title(community)) %>% 
      group_by(Year = year(`Incident Date`), community) %>% 
      summarise(n = n()) %>% 
      ungroup() %>% 
      hchart('line',
             hcaes(
               x = Year,
               y = n,
               group = community)) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "No. of Incidents"))
  })
  
  output$community_bar <- renderHighchart({
    data %>% 
      separate(`Ethnic Community`, sep = ", ", into = c("community_1", "community_2", "community_3")) %>% 
      mutate(community = map2(community_1, community_2, function(x,y){
        na.omit(c(x,y))
      })) %>% 
      mutate(community = map2(community, community_3, function(x,y){
        na.omit(c(x,y))
      })) %>% 
      unnest(cols = c(community)) %>% 
      mutate(community = str_to_title(community)) %>% 
      group_by(community) %>% 
      summarise(n = n()) %>% 
      ungroup() %>% 
      arrange(desc(n)) %>% 
      hchart('bar',
             hcaes(x = community, y = n),
             color = "steelblue",
             borderColor = "black",
             name = "No. of Incidents") %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "No. of Incidents"))
  })
  
  # output$datatable <- renderDT(
  #   data,
  #   filter = "top",
  #   options = list(pageLength = 20)
  # )
  
  output$data_explorer <- renderReactable({
    data %>% 
      rename("URL" = "Article URL") %>% 
      mutate(URL = paste0("<a href='", URL,"' target='_blank'>", icon("link"),"</a>")) %>% 
      # mutate(
      #   URL = str_trunc(URL, 20)
      # ) %>% 
      reactable(
        sortable = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        compact = TRUE,
        defaultPageSize = 6,showSortable = TRUE,
        columns = list(
          URL = colDef(width = 60,
                       html = TRUE)
        )
      )
  })
  
  output$download <- downloadHandler(
    filename = function(){"data.csv"}, 
    content = function(fname){
      write.csv(data, fname, col.names = FALSE)
    }
  )
  
  output$sunburst <- renderHighchart({
    hchart(dt_sunburst, type = "sunburst")
  })
  
  output$communitygenderbar <- renderHighchart({
    hchart(dt_community_gender,
           'column',
           hcaes(
             x = 'community',
             y = 'n',
             group = '`Gender of Victim(s)`'
           ),
           dataLabels = list(enabled = TRUE)) %>% 
      hc_plotOptions(column = list(
        dataLabels = list(enabled = FALSE),
        enableMouseTracking = TRUE)
      ) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "Number of Victims"))
  })
  
  output$monthlybar <- renderHighchart({
    hchart(dt_month,
           'bar',
           hcaes(
             x = Month,
             y = n,
             color = col
           ),
           borderColor = "white",
           name = "No. of Incidents",
           dataLabels = list(enabled = TRUE)) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "Number of Incidents"))
  })
  
  
  # output$typeline <- renderPlot({
  #   data %>% 
  #     mutate(Type = ifelse(str_detect(`Type of Incident`, ","),
  #                          str_remove(`Type of Incident`, ": .+(?=,)"),
  #                          str_remove(`Type of Incident`, ": .+"))) %>% 
  #     mutate(Type = ifelse(str_detect(Type, ","),
  #                          str_remove(Type, ": .+"),
  #                          Type)) %>% 
  #     separate(Type, into = c("Type1", "Type2"), sep = ", ") %>% 
  #     mutate(Type = purrr::map2(Type1, Type2, function(x,y){
  #       na.omit(c(x, y))
  #     })) %>% 
  #     unnest(cols = c(Type)) %>% 
  #     group_by(Year = year(`Incident Date`), Type) %>% 
  #     summarise(n = n()) %>% 
  #     ggplot(aes(Year, n, col = Type)) +
  #     geom_line(size = 0.8) +
  #     labs(x = "", y = "No. of Incidents", title = "Yearly Type of Incidents") +
  #     theme_minimal()
  # })
}

