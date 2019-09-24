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


## weekly data
## read in the baseline data
## the data is cleaned with the cleaning script
baseline <- readr::read_csv("data/processed/baseline_cleaned.csv") %>% 
  as_factor()
baseline_placed <- readr::read_csv("data/processed/placed_merged_baseline.csv") 
## monthly survey
## read in the monthly and weekly data
monthly <- readr::read_csv("data/processed/monthtly_clean.csv")
weekly <- readr::read_csv("data/processed/weekly_cleaned.csv")

## read in the placed youth data
placed_youth_unique <- read_csv("data/processed/placed_youth_unique.csv",guess_max = 1000) %>% 
  mutate(youth_id_num=`Youth IDNumber`) 
## baseline merge

placed_baseline <- baseline %>% 
  inner_join( placed_youth_unique,by=c("user_id"='youth_id_num'))

## merge weekly 
placed_weekly <- weekly %>% 
  inner_join( placed_youth_unique,by=c("user_id"='youth_id_num'))

## merge monthtly 
placed_monthly <- monthly %>% 
  inner_join( placed_youth_unique,by=c("user_id"='youth_id_num'))

shinyServer(function(input, output, session) {

  ##---------------------------------------------------------
  ##Baseline Questionaires
  ##---------------------------------------------------------
  output$tot_baseline <-renderInfoBox({
    total_baseline <- nrow(placed_baseline)
    baseline_youths <- placed_baseline %>%
      summarise(N=n_distinct(user_id))%>% unlist()
    
    infoBox( "Baseline Surveys Done",
             value = tags$p(style = "font-size: 22px; color: red",  paste0(total_baseline )), 
             subtitle = paste0("by ", baseline_youths ," youths") ,
             icon = icon("thumbs-up", lib = "glyphicon"),
             width = 4,fill = TRUE)
  })  
  
##---------------------------------------------------------
##Weekly Questionaires
##---------------------------------------------------------
output$tot_weekly <-renderInfoBox({
    total_weekly <- nrow(placed_weekly)
    weekly_youths <- placed_weekly %>%
      summarise(N=n_distinct(user_id)) %>% unlist()
    
    infoBox("Weekly Surveys Done ", 
            value = tags$p(style = "font-size: 15px;",  paste0(total_weekly )),
            subtitle = paste0("by ", weekly_youths," youths") ,
            icon = icon("thumbs-up", lib = "glyphicon"),
            width = 4,fill = TRUE)
})  

##---------------------------------------------------------
##Monthly Questionaires
##---------------------------------------------------------
output$tot_monthly <-renderInfoBox({
  total_monthly <- nrow(placed_monthly)
 monthly_youths <- placed_monthly %>%
    summarise(N=n_distinct(user_id)) %>% unlist()
  
  infoBox( "Monthly Surveys Done",
           value = tags$p(style = "font-size: 15px;",  paste0(total_monthly )), 
          subtitle = paste0("by ", monthly_youths ," youths") ,
          icon = icon("thumbs-up", lib = "glyphicon"),
          width = 4,fill = TRUE)
})  

##---------------------------------------------------------
##Phones delivered
##---------------------------------------------------------
phone_ownership <- reactive({
  phone_ownership  <- placed_youth_unique %>% 
    group_by(phone_deliver) %>% 
    summarise(n_own_phone=n())
})

output$phones_delivered <-renderPlotly({
  phone_ownership <- phone_ownership()
  plot_ly(data=phone_ownership, labels = ~`phone_deliver`, values = ~n_own_phone ,
          textinfo = 'label+percent+value') %>%
    add_pie(hole = 0.5) %>%
    layout(title = "",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T))
})  


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
    layout(title = "",  showlegend = F,
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
age_gender <- reactive({ 
 age_gender  <- placed_youth_unique %>% 
  group_by(gender_new, age_in_yrs) %>% 
  summarise(count=n()) %>% 
  mutate(perc = round((count/sum(count)) * 100,2)) %>% 
   gather(variable, value, -(gender_new:age_in_yrs)) %>%
   unite(temp, gender_new, variable) %>% 
  spread(temp , value)
})

output$plot_age_gender <- renderPlotly({
age_gender <- age_gender()
plot_ly(age_gender, x = ~age_in_yrs, y = ~female_perc, type = 'bar', name = 'Females', 
        text = ~female_count , marker = list(color = '#708fb2')) %>%
  add_trace(y = ~male_perc, name = 'Males', marker = list(color = '#19222b'), 
            text = ~male_count ) %>%
  layout(xaxis = list(title = "Age in years", tickangle = -45),
         yaxis = list(title = "Percentage by gender total"),
         margin = list(b = 100),
         title="",
         barmode = 'group' ,
         legend = list(x = 0.05, y = 0.9)) 
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
  educ_gender <-  baseline_placed %>% 
  group_by(gender_new, educ_level) %>% 
  summarise(count=n()) %>% 
    mutate(perc = round((count/sum(count)) * 100,2)) %>% 
    gather(variable, value, -(gender_new:educ_level)) %>%
    unite(temp, gender_new, variable) %>% 
    spread(temp , value)

  

   
})

output$plot_educ_gender <- renderPlotly({
  educ_gender <- educ_gender()
  educ_gender$educ_level <- factor(educ_gender$educ_level, 
                                   levels = c("Some high school", "Matric","Technical", "Honors", "Others"))
plot_ly(educ_gender, x = ~educ_level, y = ~female_perc, type = 'bar', name = 'Females', 
        text = ~female_count ,marker = list(color = '#708fb2')) %>%
  add_trace(y = ~male_perc, text = ~male_count ,name = 'Males', marker = list(color = '#19222b')) %>%
  layout(xaxis = list(title = "Education Level", tickangle = -45),
         yaxis = list(title = "Percentage by gender total"),
         margin = list(b = 100),
         title="",
         barmode = 'group' ,
         legend = list(x = 0.05, y = 0.9))
})

})