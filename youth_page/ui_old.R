library(leaflet)
library(flexdashboard)
#library(billboarder)
shinyUI(
  dashboardPage(#theme = shinytheme("sandstone") ,
    dashboardHeader(title = "Youth Dashboard"), #dashboardHeader(title = "Menu Select")
    
    dashboardSidebar(disable = T),
    dashboardBody(
      fluidRow(
          column(width = 6,
                 box(width = NULL, solidHeader = TRUE, #,
                     "Gender distribution baseline",
                     plotlyOutput("plot_gender_placed") #leafletOutput("busmap", height = 500)
                 ),
                 box(width = NULL,
                     "Gender distribution baseline",
                     plotlyOutput("plot_gender_baseline"),
                     p(
                       class = "text-muted",
                       paste("Note: not all youths filled up baseline." )
                 )
          )),
          column(width = 6,
                 box(width = NULL,
                     "Gender distribution placed",
                     plotlyOutput("plot_gender_placed")
                    ),
                 box(width = NULL, status = "warning",
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
                     plotlyOutput("plot_gender_placed"),
                     actionButton("refresh", "Refresh now"),
                     p(class = "text-muted",
                       br(),
                       "Source data updates every 4 hours."
                     )
                 )
          )
        )
      )
  )
)

# library(shinydashboard)
# library(plotly)
# library(flexdashboard) 

# 
# body <- dashboardBody(
#   fluidRow(
#     column(width = 6,
#            box(width = NULL, solidHeader = TRUE, #,
#                "Gender distribution baseline",
#                plotlyOutput("plot_gender_placed") #leafletOutput("busmap", height = 500)
#            ),
#            box(width = NULL,
#                "Gender distribution baseline",
#                plotlyOutput("plot_gender_baseline"),
#                p(
#                  class = "text-muted",
#                  paste("Note: not all youths filled up baseline." )
#            )
#     )),
#     column(width = 6,
#            box(width = NULL,
#                "Gender distribution placed",
#                plotlyOutput("plot_gender_placed")
#               ),
#            box(width = NULL, status = "warning",
#                selectInput("interval", "Refresh interval",
#                            choices = c(
#                              "30 seconds" = 30,
#                              "1 minute" = 60,
#                              "2 minutes" = 120,
#                              "5 minutes" = 300,
#                              "10 minutes" = 600
#                            ),
#                            selected = "60"
#                ),
#                plotlyOutput("plot_gender_placed"),
#                actionButton("refresh", "Refresh now"),
#                p(class = "text-muted",
#                  br(),
#                  "Source data updates every 4 hours."
#                )
#            )
#     )
#   )
# )
# 
# dashboardPage(
#   header,
#   dashboardSidebar(disable = TRUE),
#   body
# )