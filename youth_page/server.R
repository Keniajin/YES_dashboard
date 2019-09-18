library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyr)
library(readr)
## youth data
library(readxl)
library(plotly)
library(flexdashboard)

## remove the exponetial notation of the data
options(scipen = 999)
## gnowbe ned bank
gnowbe_nedbank <- readxl::read_xlsx("data/raw/gnowbedata/gnowbe_nedbank.xlsx")

## weekly data
## read in the baseline data
## the data is cleaned with the cleaning script
baseline <- readr::read_csv("data/processed/baseline_cleaned.csv")
baseline_placed <- readr::read_csv("data/processed/placed_merged_baseline.csv") 
## monthly survey
## read in the monthly and weekly data
monthtly <- readxl::read_xlsx("data/raw/from_mandla/surveyData_monthly.xlsx")
weekly <- readxl::read_xlsx("data/raw/from_mandla/surveyData_weekly.xlsx")

## read in the placed youth data
placed_youth_unique <- read_csv("data/processed/placed_youth_unique.csv",guess_max = 1000)

shinyServer(function(input, output, session) {
  
  ## create a gender age table
gender_baseline <- reactive({
    gender_baseline <- baseline %>% 
      select(`question_demo_1`) %>% 
      group_by(`question_demo_1`) %>% 
      summarise(gender=n()) 
  })
  
  ## create the plot of age and gender
output$plot_gender_baseline <- renderPlotly({
    gender_baseline <- gender_baseline()
    plot_ly(data=gender_baseline, labels = ~`question_demo_1`, values = ~gender ,
            textinfo = 'label+percent+value') %>%
      add_pie(hole = 0.6) %>%
      layout(title = "",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T))
    
  })
  
  ## create a gender table and pie for the placed youth
gender_placed <- reactive({ 
    placed_youth_unique %>% ungroup() %>% 
      select(gender_new) %>% 
      group_by(`gender_new`) %>% 
      summarise(gender=n()) 
    })
  ## a plot of the placed gender
output$plot_gender_placed <- renderPlotly({
  gender_placed <- gender_placed()
    plot_ly(data=gender_placed, labels = ~`gender_new`, values = ~gender ,
          textinfo = 'label+percent+value') %>%
    add_pie(hole = 0.6) %>%
    layout(title = "Gender distribution of placed youth",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T))

  })

output$plot_gender_placed2 <- renderPlotly({
  gender_placed <- gender_placed()
  plot_ly(data=gender_placed, labels = ~`gender_new`, values = ~gender ,
          textinfo = 'label+percent+value') %>%
    add_pie(hole = 0.6) %>%
    layout(title = "",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T))

})

### ----------------age gender----------------------------
## 
age_gender <- reactive({ placed_youth_unique %>% 
  group_by(gender_new, age_in_yrs) %>% 
  summarise(count=n()) %>% 
  spread(gender_new , count)
})

output$plot_age_gender <- renderPlotly({
  age_gender <- age_gender()
plot_ly(age_gender, x = ~age_in_yrs, y = ~female, type = 'bar', name = 'Females', 
        marker = list(color = '#708fb2')) %>%
  add_trace(y = ~male, name = 'Males', marker = list(color = '#19222b')) %>%
  layout(xaxis = list(title = "Age in years", tickangle = -45),
         yaxis = list(title = "Count"),
         margin = list(b = 100),
         title="",
         barmode = 'group')
})

output$plot_age_age_mean <- renderPlotly({
  plot_ly(placed_youth_unique %>% filter(!is.na(gender_new)), 
        y = ~age_in_yrs, 
        color = ~gender_new,
        split = ~gender_new , 
        type = "violin" ,
        box = list(visible = T),
        meanline = list(visible = T   ))
})


reload_data <- reactive({
    input$refresh # Refresh if button clicked
    
    placed_youth_unique %>% ungroup() %>% 
      select(gender_new) %>% 
      group_by(`gender_new`) %>% 
      summarise(gender=n()) 
    
    # Get interval (minimum 30)
   # interval <- max(as.numeric(input$interval), 30)
    # Invalidate this reactive after the interval has passed, so that data is
    # fetched again.
   # invalidateLater(interval * 1000, session)
    
    #getMetroData("VehicleLocations/0")
  })


## plot eduction level and gender
#--------------------------------
educ_gender <- reactive({
  baseline_placed %>% 
  group_by(gender_new, educ_level) %>% 
  summarise(count=n()) %>% 
  spread(gender_new , count)
})

output$plot_educ_gender <- renderPlotly({
  educ_gender <- educ_gender()
plot_ly(educ_gender, x = ~educ_level, y = ~female, type = 'bar', name = 'Females', 
        marker = list(color = '#708fb2')) %>%
  add_trace(y = ~male, name = 'Males', marker = list(color = '#19222b')) %>%
  layout(xaxis = list(title = "Age in years", tickangle = -45),
         yaxis = list(title = "Count"),
         margin = list(b = 100),
         title="",
         barmode = 'group')
})

})