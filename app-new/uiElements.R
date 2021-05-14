library(shiny)
library(bs4Dash)
library(highcharter)

# --- Utility functions -------------------------------------------------------

# Accordion menus
accordionMenu <- function(id, title, content) {
  
  header_id <- paste(id, "header", sep = "-")
  body_id <- paste(id, "body", sep = "-")
  
  card_header <- div(
    class = "accordion-header",
    id = header_id,
    tags$button(
      class = "accordion-button collapsed",
      type = "button",
      `data-toggle` = "collapse",
      `data-target` = paste("#", body_id, sep = ""),
      `aria-expanded` = "false",
      `aria-controls` = body_id,
      title
    )
  )
  
  card_body <- div(
    id = body_id,
    class= "accordion-collapse collapse",
    `aria-labelledby` = header_id,
    `data-parent` = paste("#", id, sep = ""),
    div(
      class = "accordion-body",
      content
    )
  )
  
  div(
    class = "accordion accordion-menu",
    id = id,
    div(
      class = "accordion-item",
      card_header,
      card_body
    )
  )
}


# --- Navigation elements -----------------------------------------------------


# Items in top header
topHeaderItems <- function() {
  tags$a(
    href = "http://www.canadianculturalmosaicfoundation.com/",
    tags$img(
      border = "0",
      alt = "CCMF Home",
      name = "CCMF Home",
      src = "./ccmf-logo.png",
      width = "55px",
      height = "35px"
    )
  )
}

# Breadcrumb navigation
navBreadcrumb <- function() {
  HTML(
    '<div id="breadcrumb-main">
    <nav aria-label="breadcrumb">
  <ol class="breadcrumb">
    <li class="breadcrumb-item"><a href="http://www.canadianculturalmosaicfoundation.com/"><img border="0" alt="CCMF Home" name = "CCMF Home" src="./ccmf-logo.png" width="30px" height="19px"> CCMF Home</a></li>
    <li class="breadcrumb-item"><a href="http://www.canadianculturalmosaicfoundation.com/quantifying-racism-in-canada.html">Racism in Canada</a></li>
    <li class="breadcrumb-item active" aria-current="page">Dashboard</li>
  </ol>
</nav>
</div>
    '
  )
}

# Back to main project page
backToProject <- function() {
  p(
    id = "back-to-project",
    "For project overview and details about the data, please see the  ",
    a(
      href = "http://www.canadianculturalmosaicfoundation.com/quantifying-racism-in-canada.html",
      target = "_blank",
      "main project page"
    ),
    "."
  )
}


navPageContents <- function() {
  
  items <- tags$ul(
    tags$li(a(href = "#user-options", "Selection options")),
    tags$li(a(href = "#overview", "Overview")),
    tags$li(a(href = "#map-view", "Racist incidents map")),
    tags$li(a(href = "#victim-trends", "Trends about victims of racist incidents")),
    tags$li(a(href = "#incident-trends", "Trends about racist incidents")),
    tags$li(a(href = "#data-explorer-view", "Data explorer"))
  )
  
  menu <- accordionMenu(
    id = "page-contents-menu",
    title = "Contents",
    content = tags$nav(
      id = "page-contents-nav",
      items
    )
  )
  
  fluidRow(
    column(
      width = 4,
      menu
    )
  )
}



# Jump to top button

jumpTopButton <- function() {
  # Sticky footer with jump to top button
  # -- other icon option: <i class="fas fa-angle-double-up"></i>
  HTML(
    '<button id="jump-top" onclick="topFunction()" title="Go to top">
         <i class="fa fa-chevron-up"></i>
      </button>'
  )
}

# --- User input widgets ------------------------------------------------------

# Widget choices are all placeholders which are updated in server.R when
# the data is loaded

userOptionsHeader <- function() {
  fluidRow(
    column(
      width = 10,
      h2("Selection options", id = "user-options"),
      p("Select the time period, province and additional options to narrow down your search, or look at the trends across the country without any selection.")
    ),
    column(
      width = 2,
      actionButton("summary_reset", "Reset")
      # tags$button("Reset", id = "summary_reset", class = "btn")
    )
  )
}

userOptionsMain <- function() {
  fluidRow(
    id = "main-options",
    column(
      width = 6,
      selectInput(
        "summary_year",
        "Select time period",
        choices = list("All", 2018, 2019, 2020, "Custom date range"),
        selected = "All",
      ),
      conditionalPanel(
        condition = "input.summary_year == 'Custom date range'",
        dateRangeInput(
          "summary_date_range",
          label = NULL,
          start = "2018-01-01",
          end = "2020-12-18",
          min = "2018-01-01",
          max = "2020-12-31"
        )
      ),
    ),
    column(
      width = 6,
      selectInput(
        "summary_province",
        "Select province",
        choices = list("All", "N/A"),
        selected = "All",
      )
    )
  )
}


userOptionsExtra <- function() {
  
  content <- tagList(
    fluidRow(
      selectInput(
        "summary_city",
        "Municipality",
        choices = c("Calgary", "Vancouver"),
        multiple = TRUE
      ),
      
    ),
    
    hr(),
    
    fluidRow(
      
      column(
        width = 6,
        h5("Victim characteristics"),
        
        selectInput(
          "summary_ethnic_community",
          "Ethnic communities",
          choices = list("N/A"),
          multiple = TRUE,
        ),
        
        selectInput(
          "summary_identity_based",
          "Identity-based intersectionalities",
          choices = list("N/A"),
          multiple = TRUE,
        ),
        
        selectInput(
          "summary_gender",
          "Gender",
          choices = c("Female", "Male", "Other/Unknown", "N/A"),
          multiple = TRUE,
        ),
      ), 
      
      column(
        width = 6,
        
        h5("Incident characteristics"),
        
        selectInput(
          "summary_incident_category",
          "Criminal vs. non-criminal",
          choices = c("Criminal", "Non-Criminal"),
          multiple = TRUE,
        ),
        
        selectInput(
          "summary_incident_type",
          "Type of incident",
          choices = list("Other"),
          multiple = TRUE
        ),
        
        selectInput(
          "summary_context",
          "Incident occurred in the context of",
          choices = list("N/A"),
          multiple = TRUE
        ),
      )
    )
  )
  
  accordionMenu(
    id = "extra-options",
    title = "Select additional options",
    content = content
  )
  
}

# --- Alert box summarizing the selected user input options -------------------
userOptionsAlert <- function() {
  htmlOutput("filters_alert")
}

# --- Data visualization helper functions -------------------------------------

chartInfoDetails <- function(chart_info_details, details_id) {
  tagList(
    div(
      class = "collapse chart-details",
      id = details_id,
      chart_info_details
    ),
    
    tags$button(
      class = "btn-link btn-more-less collapsed",
      id = paste(details_id, "button", sep = "-"),
      `data-toggle` = "collapse",
      `data-target` = paste("#", details_id, sep = ""),
      `aria-expanded` = "false",
      `aria-controls` = details_id,
      "Show more"
    ),
  )
}

chartBox <- function(title = NULL, chart_output = NULL, chart_info = NULL,
                     chart_info_details = NULL, details_id = NULL,
                     custom_content = NULL, width = 6, collapsible = FALSE, 
                     maximizable = FALSE){
  # Create a box object with chart output and info below it (default), or
  # if `custom_content` is not null, then the box will contain the custom
  # content arrangement (fluidRows and columns).
  
  details <- if (!is.null(chart_info_details)) {
    chartInfoDetails(chart_info_details, details_id)
  } else {
    NULL
  }
  
  box_content <- if (!is.null(custom_content)) {
    custom_content
  } else {
    tags$figure(
      chart_output,
      tags$figcaption(chart_info, details)
    )
  }
  
  box_output <- box(
      title = title,
      width = width,
      closable = FALSE,
      collapsible = collapsible,
      maximizable = maximizable,
      box_content
      #tagList(chart_output, chart_info, details, custom_content)
    )
  
  box_output
  
}

# --- Data visualization elements ---------------------------------------------

timeseriesBox <- function(width = 8, height = "300px") {
  chartBox(
    #title = "Trends over time",
    title = NULL,
    width = width,
    chart_output = highchartOutput("line_monthly", height = height)
  )
}

mapBox <- function(width = 12) {
  
  main_caption <- p('Above is a map of racist incidents happening across Canada. Use the widgets to select how the data is summarized (by province or by municipality). You can further customize how the data is displayed by selecting which metric (total incidents or incidents per 100,000 people) to use for shading (province) or circle size (municipality). Hover or click on a province or municipality to view the data.')
  
  details <- div(
    class = "collapse chart-details notes",
    id="details-map",
    p("When viewing the data summarized by municipality, if there are multiple overlapping circles in your area of interest, keep zooming in the map until the circles display separately."),
    p(class = "list-title", "Additional Notes"),
    tags$ul(
      tags$li("Incidents per 100,000 people are calculated based on population data from Statistics Canada\'s 2016 Census."),
      tags$li('Add a note about how municipalities were defined, which ones were included and excluded.'),
      tags$li('Add a caveat about comparing provinces and municipalities esp. when number of incidents is small.')
    )
  )
  
  custom_content <- tags$figure(
    fluidRow(
      p("Racist incidents map", class = "leaflet-title")
    ),
    fluidRow(
      column(
        width = 10,
        leafletOutput("map"),
        br()
        
      ),
      column(
        width = 2,
        radioButtons(
          "summary_map_level",
          "Layer to display",
          choices = list("Province" = "province",
                         "Municipality" = "city")
        ),
        radioButtons(
          "summary_map_metric",
          "Shading",
          choices = list("Total incidents" = "total", 
                         "Incidents per 100,000 people" = "percapita")
        ),
        conditionalPanel(
          condition = "input.summary_map_level == 'province'",
          br(),
          checkboxInput("summary_map_legend", "Show legend", value = TRUE)
        )
      ) # End of column
    ), # End of fluidRow
    
    tags$figcaption(
      main_caption,
      details,
      tags$button(
        class = "btn-link btn-more-less collapsed",
        id = "details-map-button",
        `data-toggle` = "collapse",
        `data-target` ="#details-map",
        `aria-expanded` ="false",
        `aria-controls` = "details-map",
        "Show more"
      )
    )
  )
  
  chartBox(
    # title = "Racist incidents map",
    title = NULL,
    width = width,
    custom_content = custom_content
  )
}


ethnicCommunityBox <- function(width = 6){
  chartBox(
    #title = "Ethnic communities",
    width = width,
    chart_output = highchartOutput("bar_ethnic_community"),
    chart_info = p("Click on an ethnic community to see its breakdown into sub-communities (where applicable)."),
    chart_info_details = HTML(
    '<ul class="notes">
      <li>The N/A category applies when (1) the race of the victim is not mentioned or (2) the victim(s) are racialized people in general.</li>
      <li>Totals by sub-community generally will not equal the total for the main community because the sub-community is often unknown or not applicable (e.g. an incident targeting the East Asian community, without targeting any specific sub-community).</li>
      <li>If a community contains more than 20 sub-communities, its breakdown chart displays only the top 20 sub-communities.</li>
    </ul>'
    ),
    details_id = "details-ethnic-community"
  )
}

ethnicSubCommunityBox <- function(width = 6) {
  chartBox(
    #title = "Comparison between sub-ethnic communities",
    width = width,
    chart_output = highchartOutput("bubble_ethnic_community_sub", height = "500px"),
    chart_info = p("Hover or click on a circle to view data."),
    chart_info_details = HTML(
      '<ul class="notes">
        <li>Groups of circles can be hidden or displayed by clicking on the group name in the legend.</li>
        <li>Circles can be rearranged by dragging to a new position (desktop only).</li>
      </ul>'
    ),
    details_id = "details-ethnic-community-sub"
  )
}

genderBox <- function(width = 6) {
  chartBox(
    # title = "Gender",
    width = width,
    chart_output = highchartOutput("bar_gender"),
    chart_info = p("The gender of the victim as identified in the reference article."),
    chart_info_details = HTML(
    '<p class="notes">Female includes female-born and transwomen; male includes male-born and transmen; other/unknown includes any gender outside of the previously mentioned, as well as if the article does not reference the gender of the victim. Excludes incidents where gender is not applicable (e.g. vandalism in a public space where no specific person(s) is the victim). Some incidents have multiple victims and are counted in more than one gender category, so totals may exceed 100%. Furthermore, due to limited data in media reports, we cannot further breakdown the Other/Unknown gender.
    </p>'
    ),
    details_id = "details-gender"
  )
}

identityBasedBox <- function(width = 6) {
  chartBox(
    # title = "Identity-based intersectionalities",
    width = width,
    chart_output = highchartOutput("bar_identity"),
    chart_info = p('Hover (long press on mobile) on a bar to view definitions or click to view the breakdown into sub-categories (where applicable). Due to limited data, only the intersectionalities listed above are captured.')
  )
}

incidentCategoryBox <- function(width = 6) {
  chartBox(
    # title = "Criminal vs. non-criminal",
    width = width,
    chart_output = highchartOutput("donut_category"),
    chart_info = p('Hover (long press on mobile) on a category to view definitions. Click on the "Criminal" category (where applicable) to see the breakdown of crimes by status (Charged/Not charged/Unknown). These are incidents deemed criminal by Police Services, while other incidents may be against the law but aren\'t mentioned in the article.')
  )
}

incidentTypeBox <- function(width = 6) {
  chartBox(
    # title = "Type of incident",
    width = width,
    chart_output = highchartOutput("bar_incident_type"),
    chart_info = p("Hover (long press on mobile) on a bar to view definitions or click to view the breakdown into sub-categories (where applicable)."),
    chart_info_details = HTML(
    '<p class="notes">Totals by sub-category may not equal the total for the main category because multiple sub-categories may be applicable (e.g. an incident of hate speech involving both imagery and letter/flyer sub-categories).</p>'
    ),
    details_id = "details-incident-type"
  )
}

contextBoxTreemap <- function(width = 6, height = "600px") {
  chartBox(
    # title = "(OPTION B) Incident occurred in the context of:",
    width = width,
    chart_output = highchartOutput("treemap_context", height = height),
    chart_info = p("Hover (long press on mobile) on a category to view data or click to view its breakdown into sub-categories (where applicable)."),
    chart_info_details = HTML(
      '<ul class="notes">
      <li>Totals by sub-category may not equal the total for the main category because multiple sub-categories may be applicable.</li>
    </ul>'
    ),
    details_id = "details-context"
  )
}

dataExplorerBox <- function(width = 12) {
  
  info <- HTML('<p>This data table is filtered based on the dashboard selections in the <a href="#user-options">Selection options</a> section, as well as any search terms entered in the table search bars above. To view/download the full dataset, make sure the dashboard selections are clear by clicking the <a href="#user-options">reset button</a> and clear any values from the table search bars.</p>')
  
  details <- p('The "Month" column in the table is the month (actual or estimated) during which the incident occurred. When the date/month is not specified in the article, the month is estimated based on the publication date and any other information available in the article about how long ago the incident occurred.')
  
  chartBox(
    title = NULL,
    width = width,
    custom_content = tagList(
      reactableOutput("data_explorer"),
      # br(),
      fluidRow(downloadButton("download", "Download Data")),
      # br(),
      info,
      chartInfoDetails(details, details_id = "details-data-explorer")
    )
  )
}

# --- Sandbox ----------------------------------------------------------------

# contextBoxBar <- function(width = 6) {
#   chartBox(
#     # title = "(OPTION A) Incident occurred in the context of:",
#     width = width,
#     chart_output = highchartOutput("bar_context"),
#     chart_info = p("Previous version.")
#   )
# }
# 
# 
# genderBoxAlternate <- function(width = 6) {
#   chartBox(
#     title = "Gender",
#     width = width,
#     chart_output = fluidRow(
#       highchartOutput("donut_gender", height = "300px")
#     ),
#     chart_info = p("Donut chart -- this method double counts incidents with multiple genders and can be misleading/confusing when the data is filtered to a small number of incidents.")
#   )
# }

tabBoxExample <- function() {
  fluidRow(
    bs4TabCard(
      id = "tabcard",
      # title = "A card with tabs",
      closable = FALSE,
      bs4TabPanel(
        tabName = "Tab 1", 
        active = TRUE,
        highchartOutput("line_monthly2")
      ),
      bs4TabPanel(
        tabName = "Tab 2", 
        active = FALSE,
        highchartOutput("bar_ethnic_community_sub"),
        p("Top 20 only are displayed.")
      )
    ),
  )
}

# timeseriesBoxAlternate <- function() {
#   chartBox(
#     title = "Trends over time",
#     chart_output = highchartOutput("line_monthly2", height = "250px")
#   )
# }
