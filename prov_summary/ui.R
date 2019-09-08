library(leaflet)
library(flexdashboard)
#library(billboarder)
shinyUI(
  dashboardPage(#theme = shinytheme("sandstone") ,
    dashboardHeader(), #dashboardHeader(title = "Menu Select")
    
    dashboardSidebar(width="20%",
                     column(12,
      # shinyFiles::shinyDirButton("folderChoose",
      #                            "Chose directory with the data",
      #                            "Upload"),
      # br(),
      # uiOutput("dir"),
      # br(),
      # absolutePanel(numericInput(
      #   "num",
      #   h4("Input the number of mini arrays"),
      #   value = 2
      # ),
      # br(),
      # numericInput(
      #   "num",
      #   h4("Input the number total number of samples "),
      #   value = 2
      # )),
      # 
      sidebarMenu(
        menuItem("Summaries", tabName = "dashboard"),
        menuItem("Province raw data", tabName = "rawdata")
      )
    )),
    dashboardBody(
      tabItems(
        tabItem("dashboard",
                
                fluidRow(
                  ## output the money in
                column(3, class="gauge_bg" ,
                   uiOutput("total_injection"),
                  gaugeOutput("exact_injection" ,
                              width = "100%", height = "auto")),#,
                 ##youth commitment
                 column(3, uiOutput("youth_commited"),
                  gaugeOutput("youth_placed" ,
                                       width = "100%", height = "auto")),
                 ##
                 column(3,uiOutput("total_companies"),
                 gaugeOutput("registered_perc" ,
                                       width = "100%", height = "auto")),
                column(3,uiOutput("less_earn"),
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
              DT::DTOutput("tbl")
        )
      )
    )
  )
)