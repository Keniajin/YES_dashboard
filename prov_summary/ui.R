library(leaflet)
library(flexdashboard)

#library(billboarder)
shinyUI(
  dashboardPage(#theme = shinytheme("sandstone") ,
    dashboardHeader(title = tags$a(href='https://www.yes4youth.co.za/',
                                   tags$img(src='images/logo.png'))), #dashboardHeader(title = "Menu Select")
    
    dashboardSidebar(width="20%",
                     column(12,
        sidebarMenu(
        menuItem("Summaries", icon = icon("globe-africa"), tabName = "dashboard"),
        menuItem("Province raw data" ,icon = icon("tally"), tabName = "rawdata")
      )
    )),
    dashboardBody(
      tabItems(
        tabItem("dashboard",
                
                fluidRow(
                  ## output the money in
                column(3, class="gauge_bg" ,
                   uiOutput(class="total_inject" ,"total_injection"),
                  gaugeOutput("exact_injection" ,
                              width = "100%", height = "auto")),#,
                 ##youth commitment
                 column(3, uiOutput(class="youth_commmit" ,"youth_commited"),
                  gaugeOutput("youth_placed" ,
                                       width = "100%", height = "auto")),
                 ##
                 column(3,uiOutput(class="companies" ,"total_companies"),
                 gaugeOutput("registered_perc" ,
                                       width = "100%", height = "auto")),
                column(3,uiOutput(class="youth_earn" ,"less_earn"),
                       gaugeOutput("earn_gauge" ,
                                   width = "100%", height = "auto"))
                  # uiOutput("total_placed_2"),
                  # column(3,gaugeOutput("n_placed_gauge" ,
                  #                      width = "100%", height = "auto")) ,
                  # uiOutput("total_placed_2"),
                  # column(3,gaugeOutput("n_placed_gauge" ,
                  #                      width = "100%", height = "auto")) 
                 
                  #column(class="gauge2",6,valueBoxOutput("total_youth")),
                  #column(class="gauge2",6,valueBoxOutput("total_placed"))#,
                  #column(3,valueBoxOutput(""))
                 #flexdashboard::gaugeOutput("n_placed_gauge") 
                ),
                fluidRow(
                  box(#width = 8, status = "info", solidHeader = TRUE,
             
                    div(class="map_plot" ,
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        
                        "Region summary",
                       # plotOutput('data_process', width="100%"),
                       leafletOutput("map", width = '100%' ))
                    #leafletOutput("map", width="100%", height="100%")
                  ),
                  box(
                    div(class="map_plot" ,
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        
                        "Company sign up status",
                        plotlyOutput("plot_registered"))
                  )
                )
        ),
        tabItem("rawdata",
              #  numericInput("maxrows", "Rows to show", 25),
                #verbatimTextOutput("rawtable"),
                #downloadButton("downloadCsv", "Download as CSV")
              DT::DTOutput("tbl"),
              downloadLink('downloadData', 'Download data')
        )
      )
    )
  )
)