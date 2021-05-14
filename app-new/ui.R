

ui <- bs4DashPage(
  
  title = "Racism in Canada",
  
  dashboardHeader(skin = "dark", id = "top-header", topHeaderItems()),
  
  sidebar = dashboardSidebar(disable = TRUE),
  
  body = dashboardBody(
    
    # Bootstrap icons
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.4.1/font/bootstrap-icons.css"
    ),
    
    # Custom styles
    includeCSS("./www/accordion.css"),
    includeCSS("./www/styles.css"),
    
    navBreadcrumb(),
  
    h1("Racism in Canada"),

    p(id = "page-subtitle",
      "Trends reported from mainstream English-language Canadian media 2018 - 2022"),
    
    backToProject(),
    
    navPageContents(),
    
    # --- User selection options ----------------------------------------------
    hr(),
    userOptionsHeader(),
    userOptionsMain(),
    userOptionsExtra(),
    userOptionsAlert(),
    
    # --- Output and plots ---------------------------------------------------
    hr(),
    h2("Overview", id = "overview"),
    
    # Scorecard and monthly time series
    fluidRow(
      valueBoxOutput("vbox_total", width = 4),
      timeseriesBox(width = 8)
    ),
    
    # Map
    fluidRow(
      id = "map-view",
      mapBox(width = 12)
    ),

    hr(),
    h2("Trends about victims of racist incidents", id = "victim-trends"),
    fluidRow(
      ethnicCommunityBox(),
      ethnicSubCommunityBox(),
      genderBox(),
      identityBasedBox()
    ),

    hr(),
    h2("Trends about racist incidents", id = "incident-trends"),
    fluidRow(
      incidentCategoryBox(),
      incidentTypeBox(),
      contextBoxTreemap(width = 12)
    ),

    hr(),
    h2("Data explorer", id = "data-explorer-view"),
    dataExplorerBox(),

    hr(),
    h2("Sandbox"),

    tabBoxExample(),
    #timeseriesBoxAlternate(),

    br(),
    br(),
    
    # --- Footer --------------------------------------------------------------
    jumpTopButton(),
    
    # --- Custom scripts ------------------------------------------------------
    tags$script(src = "scripts.js")
    
  )   

)
 