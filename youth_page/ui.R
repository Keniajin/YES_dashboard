library(leaflet)
library(flexdashboard)
library(plotly)
library(shinydashboard)
#library(billboarder)
shinyUI(
  dashboardPage(#theme = shinytheme("sandstone") ,
    dashboardHeader(title = "youth Dashboard"), #dashboardHeader(title = "Menu Select")
    
    dashboardSidebar(disable = T),
    dashboardBody(
          fluidRow(
              
                  column(3, 
                       div(class="youth_page",
                         "Surveys",
                         uiOutput("tot_baseline") ,
                         uiOutput("tot_weekly"),
                         uiOutput("tot_monthly")
                       ) ) ,#,
                  ##youth commitment
                  column(3, 
                         div(class="youth_page",
                           "Gender distribution of placed youth",
                           plotlyOutput("plot_gender_placed")
                         )
                         ),
                  ##
                  column(3,
                         div(class="youth_page",
                           "Mean age by gender",
                           plotlyOutput("plot_age_age_mean")
                         )
                         ),
                  column(3,
                         div(class="youth_page",
                             "Phone Deliverly Status",
                             plotlyOutput("phones_delivered")
                         ))
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
                  box("Gender vs age in years", #width = 8, status = "info", solidHeader = TRUE,
                    
                    div(class="youth_page",
                      plotlyOutput("plot_age_gender"),
                      width = NULL, status = "warning",
                      selectInput("interval", "Refresh interval",
                                  choices = c(
                                    "30 seconds" = 30,
                                    "1 minute" = 60,
                                    "2 minutes" = 120,
                                    "5 minutes" = 300,
                                    "10 minutes" = 600
                                  ),
                                  selected = "60"
                      ),
                      
                      actionButton("refresh", "Refresh now"),
                      p(class = "text-muted",
                        br(),
                        "Source data updates every 4 hours."
                      )
                      
                    )
                    #leafletOutput("map", width="100%", height="100%")
                  ),
                  box(
                    div(class="map_plot" ,
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        
                        "Education grouped by gender",
                        plotlyOutput("plot_educ_gender")),
                    p(class = "text-muted",
                      br(),
                      "Fewer entries in the baseline data."
                    )
                  )
                )
       
      )
    )
  )
