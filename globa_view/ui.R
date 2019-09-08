shinyUI(
  dashboardPage(#theme = shinytheme("sandstone") ,
    dashboardHeader(title = "Menu Select"),
    
    dashboardSidebar(width="20%",
                     column(12,
      shinyFiles::shinyDirButton("folderChoose",
                                 "Chose directory with the data",
                                 "Upload"),
      br(),
      uiOutput("dir"),
      br(),
      absolutePanel(numericInput(
        "num",
        h4("Input the number of mini arrays"),
        value = 2
      ),
      br(),
      numericInput(
        "num",
        h4("Input the number total number of samples "),
        value = 2
      )),
      
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard"),
        menuItem("Raw data", tabName = "rawdata")
      )
    )),
    dashboardBody(
      tabItems(
        tabItem("dashboard",
                fluidRow(
                  valueBoxOutput("files_in_batch")#,
                  #valueBoxOutput("count"),
                 # valueBoxOutput("users")
                ),
                fluidRow(
                  box(
                 #   width = 8, status = "info", solidHeader = TRUE,
                  #  title = "Popularity by package (last 5 min)",
                  #  bubblesOutput("packagePlot", width = "100%", height = 600)
                    div(class="span6" , "Yearly and Monthly Admissions", 
                        plotOutput('data_process', width="100%"),
                        style = "height:80%;background-color: yellow;")
                  ),
                  box(
                   # width = 4, status = "info",
                   # title = "Top packages (last 5 min)",
                   # tableOutput("packageTable")
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