library(leaflet)
library(flexdashboard)
library(plotly)
library(shinydashboard)
#library(billboarder)
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

shinyUI(
  dashboardPage(#theme = shinytheme("sandstone") ,
    dashboardHeader(title = "youth Dashboard"), #dashboardHeader(title = "Menu Select")
    
    dashboardSidebar(width="15%",
                     column(12,
                            sidebarMenu(
                              menuItem("Youth Demographics", icon = icon("id-card-alt"), tabName = "demographics"),
                              menuItem("Engagements", icon = icon("envelope"), tabName = "engagements"),
                             # menuItem("Pyschometrics", tabName = "pyschometrics"),
                              convertMenuItem(
                                menuItem("Pyschometrics", 
                                         tabName = "pyschometrics",icon = icon("bar-chart-o"),
                                                      menuSubItem("Baseline", tabName = "baseline"),selected=T,
                                                       menuSubItem("Monthly", tabName = "monthly"),
                                                       menuSubItem("Weekly", tabName = "weekly")),
                                "pyschometrics")
                            )# close side bar menu
                     )),
    dashboardBody(
      tabItems(
#---- 
## demographics
        tabItem("demographics",
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
                        
                        plotlyOutput("plot_educ_gender")
                        ),
                    p(class = "text-muted",
                      br(),
                      "Fewer entries in the baseline data."
                    )
                  )
                ),## end fluid row
          fluidRow(
           column(6, 
                   div("Surveys Merge",
                       br(),
                      shinydashboard::valueBoxOutput("v_bmw"),
                        shinydashboard::valueBoxOutput("v_bm") ,
                       shinydashboard::valueBoxOutput("v_bw")
                       
                   ) ),
           column(6
               )
          )
       
      ),##end demographic tabitem
#---- 
## engagements
    tabItem("engagements",
            
            fluidRow(
              ## Weekly surveys summary
              column(6,
                     div(#class="youth_page",
                         "Weekly Surveys",
                         ## define a drop down for the different companies
                         ## can we split per region ? Confirm with George K
                         uiOutput('select_co') ,
                         tags$br(),
                         tags$br(),
                         plotlyOutput("weekly_survey_co")
                     )) ,
              column(6,
                     div(#class="youth_page",
                       "Monthly Surveys",
                       ## define a drop down for the different companies
                       ## can we split per region ? Confirm with George K
                       uiOutput('select_co_month') ,
                       tags$br(),
                       tags$br(),
                       plotlyOutput("monthly_survey_co")
                     )
                   
              )
            ),
            ## add a row for data download
            fluidRow(
              column(6,
                     div(#class="youth_page",
                       "Weekly downloads",
                       ## define a drop down for the different companies
                       ## can we split per region ? Confirm with George K
                    
                       tags$br(),
                       tags$br(),                      
                       downloadButton('downloadData', 'Weekly overall', class= "mybutton"),
                       downloadButton('', 'Top 20 youths weekly', class= "mybutton"),
                       downloadButton('', 'Top 20 company weekly', class= "mybutton"),
                       DT::DTOutput("weekly_table")
                     )) ,
              column(6,
                     div(
                       "Monthly downloads",
                       ## define a drop down for the different companies
                       ## can we split per region ? Confirm with George K
                       
                       tags$br(),
                       tags$br(),                      
                       downloadButton('download_monthly', 'Monthly Overall', class= "mybutton"),
                       downloadButton('', 'Top 20 youths monthly', class= "mybutton"),
                       downloadButton('', 'Top 20 company monthly', class= "mybutton"),
                       
                       DT::DTOutput("monthly_table")
                     )
                     )
                     
           
            )
    ),## end engagements tab item
#---- 
## engagements
#---- 
tabItem("baseline",
      fluidRow(
          ## Basline pyschometrics
          column(6,
                 div(#class="youth_page",
                   "Baseline Big Five:",
                   #plotlyOutput("baseline_psy_big_5")
                   plotOutput("baseline_psy_big_5")
                 )) ,
         
        
          column(6,
                 div(#class="youth_page",
                   "Baseline Big Five Ridges:",
                   #plotlyOutput("baseline_psy_big_5")
                   plotOutput("baseline_psy_big_5_2")
                 )),
          column(6,
                 "Self-efficacy, Dealing with conflict , Self-Control and Dealing with stress",
                 div(
                   plotOutput("self_control")
                 )
                 
          ),
          column(6,
                 "Self-efficacy, Dealing with conflict , Self-Control and Dealing with stress",
                 div(
                   plotOutput("self_control_2")
                 )
                 
          )
        ),
        fluidRow(
          ## Basline pyschometrics
          column(6,
                 div(#class="youth_page",
                   "Decision-Making, Attention to Detail, Self-esteem and Grit",
                   #plotlyOutput("baseline_psy_big_5")
                   plotOutput("decision")
                 )) ,
          column(6,
                 "Internal Locus of Control, Perceived Stress Scale , Present day bias and Well-being (SPANE)",
                 div(
                   plotOutput("internal_control")
                 )
                 
          )
        ), 
        fluidRow(
          ## Basline pyschometrics
          column(6,
                 div(#class="youth_page",
                   "Attitudes towards work, Workgroup Emo Int Profile, MMCS",
                   #plotlyOutput("baseline_psy_big_5")
                   plotOutput("attdwork")
                 )) ,
          column(6,
                 #"Internal Locus of Control, Perceived Stress Scale , Present day bias and Well-being (SPANE)",
                 div(
                   #plotOutput("internal_control")
                 )
                 
          )
        )
  ),## end baseline tab item
#---- 
tabItem("monthly","Monthly Psychometrics",
        fluidRow(
          ## Weekly pyschometrics
          column(6,
                 "Dealing with conflict, Dealing with stress, Well-being (SPANE) and Perceived Stress Scale",
                 div(
                   plotOutput("conflict_month")
                 )
                 
          ),
          column(6,
                 "Belonging , Workplace , Problem solving and Behaviors",
                 div(
                   plotOutput("belong_month")
                 )
                 
          )
        ),## end fluid row 1
        fluidRow(
          ## Weekly pyschometrics
          column(6,
                 "Self-efficacy, Motivation, Team Work and Satisfaction ",
                 div(
                   plotOutput("efficacy_month")
                 )
                 
          ),
          column(6#,
                # "Belonging , Workplace , Problem solving and Behaviors",
                # div(
                 #  plotOutput("belong_month")
               #  )
                 
          )
        )
        ),##end monthly
tabItem("weekly","Weekly Pshychometrics",
        fluidRow(
          ## Weekly pyschometrics
            column(12,
                 "Check In, Behaviors and Satisfaction",
                 div(
                   plotOutput("psycho_week")
                 )
                 
          )
        )
        
        )## end weekly
)# end tab items
)## end dashboard boday
)) #end shiny UI and dashboard page
