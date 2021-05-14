

ui <- bs4DashPage(
  navbar = bs4DashNavbar(
    skin = "dark",
    rightUi = tagList(
      h5("Contact us:", style = "color:white"), HTML('&nbsp'), HTML('&nbsp'),
      HTML("<a href = 'https://www.facebook.com' style=color:white><i class='fa fa-facebook fa-lg'></i></a>"),
      HTML('&nbsp'), HTML('&nbsp'),
      HTML("<a href = 'https://www.twitter.com'style=color:white><i class='fa fa-twitter fa-lg'></i></a>"),
      HTML('&nbsp'), HTML('&nbsp'), HTML('&nbsp'), HTML('&nbsp'),
      h5("Share", style = "color:white"), HTML('&nbsp'),
      HTML("<a href = 'https://www.twitter.com' style=color:white;><i class='fa fa-share fa-lg'></i></a>"),
      HTML('&nbsp'), HTML('&nbsp'), HTML('&nbsp'), HTML('&nbsp'),
      tags$img(
        src = "./logo.jpg",
        height = "35px",
        width = "55px"
      )
      #tags$a(href = "https://www.facebook.com", tags$p(fa("facebook", height = "20px", fill = "blue")))
    )
  ),
  title = "Racism in Canada",
  
  sidebar = bs4DashSidebar(
    title = h3("Racism in Canada", style = "color:white;"),
    status = "info",
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
      menuItem("Map", tabName = "tab1", icon = icon("dashboard")),
      menuItem("Data Explorer", tabName = "data", icon = icon("dashboard")),
      menuItem("Charts", tabName = "tab2", icon = icon("dashboard")),
      menuItem("Charts 2", tabName = "charts2", icon = icon("dashboard")),
      menuItem("About Us", tabName = "tab3", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    
    includeCSS("./www/styles.css"),
    
    tabItems(
      
      tabItem(
        tabName = "summary",
        
        fluidRow(
          h1("Racism in Canada"),
        ),
        fluidRow(
          h3("Media Reported Trends")
        )
        
        # fluidRow(
        #   br(),
        #   infoBoxOutput("topcities_v1", width = 3),
        #   infoBoxOutput("topcities_v2", width = 3),
        #   infoBoxOutput("topcities_v3", width = 3),
        #   infoBoxOutput("topcities_v4", width = 3)),
        # fluidRow(
        #   box(
        #     leafletOutput("map", width = "100%", height = "700px"),
        #     width = 12,collapsible = F,closable = F,
        #     height = 750
        #   )
        # )
        
      ),
      
      tabItem(
        tabName = "tab1",
        
        fluidRow(
          br(),
          infoBoxOutput("topcities_v1", width = 3),
          infoBoxOutput("topcities_v2", width = 3),
          infoBoxOutput("topcities_v3", width = 3),
          infoBoxOutput("topcities_v4", width = 3)),
        fluidRow(
          box(
            leafletOutput("map", width = "100%", height = "700px"),
            width = 12,collapsible = F,closable = F,
            height = 750
          )
          
          # box(
          #   plotOutput("yearlycount"),
          #   width = 4
          # ),
          
          # box(plotOutput("datescountline")),
          
          
          
          
          
          #box(plotOutput("topcities"))
          
           
        )
      ),
      
      tabItem(
        tabName = "tab2",
        fluidRow(
          
          column(
            width = 2
          ),
          
          box(
            width = 8,
            title = "Table showing the listings of Type of Incident & Gender (#3)",
            fluidRow(
              column(
                width = 12,
                tags$div(
                  selectInput("province", "Province", choices = NULL, selected = NULL, width = "100%"),
                  style = "text-align:center"
                )
              ),
              
              column(
                width = 6,
                reactableOutput("table_3_1")
              ),
              column(
                width = 6,
                reactableOutput("table_3_2")
              )
            )
            
          ),
          
          box("Number of Cases in Cities",
              highchartOutput("sunburst")),
          box("Top Dates with Incidents" ,
              highchartOutput("dates_count_bar")),
          box("Monthly Number of Cases",
              highchartOutput("monthlybar")),
          box("Number of Victims in Different Communities and Gender",
              highchartOutput("communitygenderbar"))
          # box("Type of Incident",
          #     highchartOutput("typebar")),
          # box("Yearly Gender of Victim",
          #     highchartOutput("genderline")),
          # box("Gender of Victim",
          #     highchartOutput("genderbar")),
          # box("Yearly Target Community",
          #     highchartOutput("communityline")),
          # box("Target Community",
          #     highchartOutput("communitybar"))
          #box(plotOutput("typeline"))
        )
      ),
      
      
      tabItem(
        tabName = "charts2",
        fluidRow(
          box("Identity-Based",
              highchartOutput("identity_based_bar")),
          box("Yearly Number of Identity Based Crimes",
              highchartOutput("identity_based_line")),
          box("Context of Incident",
              highchartOutput("context_bar")),
          box("Yearly Context",
              highchartOutput("context_line"))
        )
      ),
      
      
      
      tabItem(
        tabName = "data",
        
        # column(
        #        width = 12,
        #        align = "center"),
        
        fluidRow(
          box(
            width = 12,
            closable = FALSE,
            collapsible = FALSE,
            reactableOutput("data_explorer"),
            br(),
            downloadButton(
              'download',
              "Download Data",
              style = "background:#17a2b8; color:white;"
            )
          )
        )
        
        #fluidRow(column(downloadButton('download',"Download Data"), width = 12), align = "center"),
        #fluidRow(DTOutput("datatable"))
      ),
      
      tabItem(
        tabName = "tab3",
        fluidRow(
          column(
            width = 2
          ),
          box(
            width = 7,
            #solidHeader = TRUE,
            status = "secondary",
            title = "What we do",
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
            Quisque tellus urna, placerat in tortor ac, imperdiet sollicitudin mi.
            Integer vel dolor mollis, feugiat sem eu, porttitor elit.
            Sed aliquam urna sed placerat euismod.
            In risus sem, ornare nec malesuada eu, ornare quis dui.
            Nunc finibus fermentum sollicitudin.
            Fusce vel imperdiet mi, ac faucibus leo.
            Cras massa massa, ultricies et justo vitae, molestie auctor turpis.
            Vestibulum euismod porta risus euismod dapibus.
            Nullam facilisis ipsum sed est tempor, et aliquam sapien auctor.
            Aliquam velit ligula, convallis a dui id, varius bibendum quam.
            Cras malesuada nec justo sed aliquet.
            Fusce urna magna, malesuada tempus pharetra in, fringilla iaculis mi.
            Morbi rutrum iaculis convallis.",
            style = "text-align:center"
          )
        )
      )
    )
    
    
      

   
  )
)

# library(bootstraplib)
# 
# #bs_theme_new(version = "4", bootswatch = "united")
# 
# ui <-tagList(
#   
#   shinyWidgets::useShinydashboard(),
#   includeCSS("./www/styles.css"),
#   
#   navbarPage(
#     id = "nav",
#     title = "Canada Crimes",
#     #bootstrap(),
#     
#     theme = shinythemes::shinytheme(theme = "paper"),
#     # 1st tab -----------------------------------------------------------------
#     tabPanel(
#       title = "Map Tab",
#       sidebarLayout(
#         sidebarPanel(
#           width = 3
#         ),
#         mainPanel(
#           valueBox(
#             value = 10,
#             subtitle = "this is it",
#             width = 3
#           ),
#           valueBox(
#             value = 10,
#             subtitle = "this is it",
#             width = 3
#           ),
#           valueBox(
#             value = 10,
#             subtitle = "this is it",
#             width = 3
#           ),
#           valueBox(
#             value = 10,
#             subtitle = "this is it",
#             width = 3
#           ),
#           box(
#             echarts4rOutput("map")
#           )
#           
#         )
#       )
#       
#     )
#     
#   )
# )
#   
#   
#   