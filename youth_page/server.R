library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(zoo)
library(aweek)
## youth data
library(readxl)
library(plotly)
library(flexdashboard)
library(DT)
library(viridis)
library(ggridges)
library(forcats)

## remove the exponetial notation of the data
options(scipen = 999)


## weekly data
## read in the baseline data
## the data is cleaned with the cleaning script
baseline <- readr::read_csv("data/processed/baseline_cleaned.csv") 
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

placed_weekly <- placed_weekly %>% 
  mutate(submitted=as_date(submitted)) %>% 
  mutate(year_month_week=format(submitted ,"%b%Y") ,
         month=month(submitted ,label = T, abbr = T) ,
         yr_week=paste0("(",month,")","wk",isoweek(submitted),"/",year(submitted)),
         yes_week = aweek::date2week(submitted,week_start = 5, floor_day = T),
         yes_week_mnth =paste0(yes_week , "(",month,")"))



## merge monthtly 
placed_monthly <- monthly %>% 
  inner_join( placed_youth_unique,by=c("user_id"='youth_id_num')) 

placed_monthly %>%
  summarise(N=n_distinct(user_id))


placed_monthly <- placed_monthly %>% 
  mutate(submitted=as_date(submitted)) %>% 
  mutate(year_month=format(submitted ,"%b%Y") ,
         month=month(submitted ,label = T, abbr = T) ,
         yr_week=paste0("(",month,")","wk",isoweek(submitted),"/",year(submitted)),
         yes_week = aweek::date2week(submitted,week_start = 5, floor_day = T))
## companies summaries
##weekly and placed youth merge
overall_weeks <- placed_weekly %>% 
  group_by(year_month_week) %>% 
  summarise(weekly_surveys=n())
overall_weeks  <- overall_weeks %>% 
  mutate(year_month_week=as.factor(zoo::as.yearmon(year_month_week , "%b%Y")))


month_id <- placed_weekly %>% 
  select(month , yes_week) %>% 
  mutate(yes_week = paste0(yes_week)) %>% 
  distinct(yes_week , month) %>% 
  group_by(yes_week) %>% 
  mutate(yes_week_month=paste0(month, collapse = "" , sep="")) %>% 
  distinct(yes_week , .keep_all = T) %>% select(-month)



overall_weeks_company <- placed_weekly %>% 
  group_by(yes_week, `Company Name`) %>% 
  summarise(weekly_surveys=n() ,
            total_youths= n_distinct(user_id) ) %>% 
  group_by(`Company Name`) %>% 
  mutate(sum_surveys=sum(weekly_surveys) , total_placed=sum(total_youths)) %>% 
  ungroup() %>% 
  mutate(rate_response=round((weekly_surveys /total_placed)*100,2)) %>% 
  mutate(yes_week=as.character(yes_week)) %>% 
  left_join(month_id, by="yes_week") %>% 
  mutate(time_survey = paste0(yes_week,"(", yes_week_month,")")) %>% 
  ## rename to allow reusability of functions
  rename( total_surveys=weekly_surveys)

## create a summary table for download for youth counter
youth_weekly <- placed_weekly %>% 
  select(YESID =user_id,YouthIDNumber=`Youth IDNumber`,Gender ,
         Province, Company_Name=`Company Name`,Placement, Start_date=`Start date`, yes_week , submitted) %>% 
  mutate(current_yes_week = aweek::date2week(Sys.Date(),week_start = 5, floor_day = T),
         current_yes_week=as.numeric(substr(current_yes_week,7,8)), 
         yes_start_week = aweek::date2week(Start_date,week_start = 5, floor_day = T),
         yes_start_week=as.numeric(substr(yes_start_week,7,8)), 
         expected_weeks=current_yes_week-yes_start_week  ,
         expected_month=month(Sys.Date()) - month(Start_date),
         expected_weekly_surveys=expected_weeks-expected_month) %>% 
  select(-c(expected_month,expected_weeks)) %>% 
  group_by(YouthIDNumber) %>%  
  mutate(weekly_survey_done=n())%>% 
  arrange(YouthIDNumber) %>% 
  distinct(YouthIDNumber , .keep_all = T)

## monthly surveys
overall_months <- placed_monthly %>% 
  group_by(year_month) %>% 
  summarise(monthly_surveys=n())  %>% 
  mutate(year_month=as.factor(zoo::as.yearmon(year_month , "%b%Y")))



overall_month_company <- placed_monthly %>% 
  group_by(year_month , `Company Name`) %>% 
  summarise(monthly_surveys=n() ,
            total_youths= n_distinct(user_id)) %>%
  ungroup() %>% 
  mutate(year_month=as.factor(zoo::as.yearmon(year_month , "%b%Y"))) %>% 
  group_by(`Company Name`) %>% 
  mutate(sum_surveys=sum(monthly_surveys),total_placed=sum(total_youths)) %>% 
  mutate(rate_response=round((monthly_surveys /total_placed)*100,2)) %>% 
  #fill(year_month , `Company Name`) %>% 
  ## rename to allow reusability of functions
  rename(time_survey=year_month, total_surveys=monthly_surveys) 


youth_monthly <- placed_monthly %>% 
  select(YESID =user_id,YouthIDNumber=`Youth IDNumber`,Gender ,
         Province, Company_Name=`Company Name`,Placement, Start_date=`Start date`, year_month , submitted) %>% 
  mutate(current_yes_month = month(Sys.Date() ,label = T, abbr = T) ,
         yes_start_month = month(Start_date,label = T, abbr = T),
         expected_month=interval(ymd(Start_date), ymd(Sys.Date()))  ,
         expected_monthly_surveys=expected_month %/% months(1))          %>% 
  select(-c(expected_month)) %>% 
  group_by(YouthIDNumber) %>%  
  mutate(monthly_survey_done=n())%>% 
  arrange(YouthIDNumber) %>% 
  distinct(YouthIDNumber , .keep_all = T)

## youths with number of surveys
baseline_weekly <- placed_baseline %>% 
  distinct(user_id , .keep_all = T)  %>% 
  select(user_id , survey_b=survey) %>% 
  inner_join(placed_weekly %>% 
               distinct(user_id , .keep_all = T)  %>% 
               select(user_id , survey_w=survey))

baseline_monthly <- placed_baseline %>% 
  distinct(user_id , .keep_all = T)  %>% 
  select(user_id , survey_b=survey) %>% 
  inner_join(placed_monthly %>% 
               distinct(user_id , .keep_all = T)  %>% 
               select(user_id , survey_m=survey))


baseline_monthly_weekly <- placed_baseline %>% 
  distinct(user_id , .keep_all = T)  %>% 
  select(user_id , survey_b=survey) %>% 
  inner_join(placed_monthly %>% 
               distinct(user_id , .keep_all = T)  %>% 
               select(user_id , survey_m=survey)) %>% 
  inner_join(placed_weekly %>% 
               distinct(user_id , .keep_all = T)  %>% 
               select(user_id , survey_w=survey))


## plotly margins
m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)

## psychometrics file
baseline_psy <- readr::read_csv("data/processed/baseline_psy.csv")
weekly_psy <- readr::read_csv("data/processed/weekly_psy.csv")
monthly_psy <- readr::read_csv("data/processed/monthly_psy.csv")

labels <- read_excel("./docs/items_structure.xlsx", sheet ="Labels") 


## merrge the placed weekly data with the metrics
placed_weekly_psy <- placed_weekly %>% 
  select(yes_week_mnth,year_month_week ,user_id,submitted,
         month, yes_week,age_in_yrs ,`Company Name`) %>% 
  left_join(weekly_psy , by =c("user_id", "submitted") ) %>% 
  group_by(user_id) %>% arrange(submitted) %>% mutate(week_rank=1:n()) %>% 
  mutate(week1_check_in=first(sc_check_in)) %>% 
  mutate(direction_wk1=sc_check_in/week1_check_in) %>% 
  mutate(direction_wk1 = ifelse(direction_wk1>0,"Increase",
                                ifelse(direction_wk1<0,"Decrease",
                                       ifelse(direction_wk1==0,"No Change","Missing")))) %>% 
  group_by(user_id) %>%
  arrange(week_rank) %>% 
  mutate(sc_check_in_lw = dplyr::lag(sc_check_in, n = 1, default = NA) ,
         direction_lw=sc_check_in-sc_check_in_lw,
         direction_lw=ifelse(direction_lw>0,"Increase",
                             ifelse(direction_lw<0,"Decrease",
                                    ifelse(direction_lw==0,"No Change","Missing")))) %>% 
  select(yes_week, week_rank,user_id,submitted, everything())


shinyServer(function(input, output, session) {

  
##---------------------------------------------------------
  ##Baseline Questionaires
##---------------------------------------------------------
  output$tot_baseline <-renderInfoBox({
    total_baseline <- nrow(placed_baseline)
    baseline_youths <- placed_baseline %>%
      summarise(N=n_distinct(user_id))%>% unlist()
    
    infoBox( tags$p(style = "font-size: 12px; color: #19222b;","Youths with baseline survey"),
             value = tags$p(style = "font-size: 22px; color: #f48632;",  paste0(baseline_youths)), 
             subtitle = tags$p(style = "font-size: 10px;",paste0("with ", total_baseline ," surveys")) ,
             icon = icon("thumbs-up", lib = "glyphicon"),
             color = "aqua",
             width = 4,fill = F)
  })  
  
##---------------------------------------------------------
##Weekly Questionaires
##---------------------------------------------------------
output$tot_weekly <-renderInfoBox({
    total_weekly <- nrow(placed_weekly)
    weekly_youths <- placed_weekly %>%
      summarise(N=n_distinct(user_id)) %>% unlist()
    
    infoBox(tags$p(style = "font-size: 12px; color: #19222b;","Youths with weekly surveys "), 
            value = tags$p(style = "font-size: 22px;  color: #708fb2;",  paste0(weekly_youths  )),
            subtitle = tags$p(style = "font-size: 10px;",paste0("with ", total_weekly," surveys")) ,
            color = "navy",
            icon = icon("thumbs-up", lib = "glyphicon"),
            width = 4,fill = F)
})  

##---------------------------------------------------------
##Monthly Questionaires
##---------------------------------------------------------
output$tot_monthly <-renderInfoBox({
  total_monthly <- nrow(placed_monthly)
 monthly_youths <- placed_monthly %>%
    summarise(N=n_distinct(user_id)) %>% unlist()
 
  infoBox( tags$p(style = "font-size: 12px; color: #19222b;",  "Youths with monthly surveys"),
           value = tags$p(style = "font-size: 22px; color: #19222b;",  paste0( monthly_youths  )), 
          subtitle = tags$p(style = "font-size: 10px;", paste0("with ",  total_monthly," surveys") ),
          color = "lime",
          icon = icon("thumbs-up", lib = "glyphicon"),
          width = 4,fill = F)
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
    filter(age_in_yrs>17 & age_in_yrs<38) %>% 
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
         margin = list(m),
         title="",
         barmode = 'group' ,
         legend = list(x = 0.05, y = 0.9)) 
})

output$plot_age_age_mean <- renderPlotly({
  placed_youth_v <- placed_youth_unique %>% 
    filter(age_in_yrs>17 & age_in_yrs<38)
  plot_ly(placed_youth_v %>% filter(!is.na(gender_new)), 
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

## education level by gender
output$plot_educ_gender <- renderPlotly({
  educ_gender <- educ_gender()
  educ_gender$educ_level <- factor(educ_gender$educ_level, 
                                   levels = c("Some high school", "Matric","Technical", "Honors", "Others"))
plot_ly(educ_gender, x = ~educ_level, y = ~female_perc, type = 'bar', name = 'Females', 
        text = ~female_count ,marker = list(color = '#708fb2')) %>%
  add_trace(y = ~male_perc, text = ~male_count ,name = 'Males', marker = list(color = '#19222b')) %>%
  layout(xaxis = list(title = "Education Level", tickangle = -45),
         yaxis = list(title = "Percentage by gender total"),
         margin = list(m),
         title="",
         barmode = 'group' ,
         legend = list(x = 0.05, y = 0.9))
})

##---------------------------------------------------------
##Weekly and monthly surveys bar graphs
##---------------------------------------------------------

## this function is utlised for weekly and monthly surveys
## generate the  surveys plots be called by the output 
plot_survey_bars <- function(company_df ,company_name , survey){
  t_youths_placed <- max(company_df$total_placed)
  if(survey=="Weekly"){
    name_plot = 'Weekly Surveys'
    xaxis_title = "Week of the year"
    yaxis_title= "Rate of weekly surveys(%)"
  }else if(survey=="Monthly"){
    name_plot = 'Monthly Surveys'
    xaxis_title = "Month of the year"
    yaxis_title="Rate of monthly surveys (%)"
  }
  ## create the plotly
  t <- list(
    family = "sans serif",
    size = 5,
    color = toRGB("red"))
  
  if( max(company_df$sum_surveys,na.rm = T) >2){
    if(nrow(company_df)>1){
      company_df <- company_df %>% ungroup()
   
      p <- plot_ly(company_df, x = ~time_survey , y = ~rate_response, type = 'bar', 
                   name = name_plot, text=~paste0(total_surveys , " by ", t_youths_placed ," placed youths"), 
                   marker = list(color = '#708fb2')) %>%
        #add_text(textfont = t, textposition = "top right") %>%
        layout(xaxis = list(title = xaxis_title, tickangle = -45 , categoryorder = "array"),
               yaxis = list(title =yaxis_title # , tickformat=',d' 
                            ),
              margin = list(m),
               title=paste0("Surveys for ", company_name)
               ) %>% 
        ## remove the download window
        config(displayModeBar = F)
    }else{
      p <- ggplot(data=company_df, aes(x=time_survey, y=rate_response)) +
        geom_bar(aes(text=paste0(total_surveys , " by ", t_youths_placed," placed youths")),
                 colour="#1E5878", stat="identity",  fill='#1E5878') + theme_bw()
      p <- ggplotly(p) %>% 
        layout(xaxis = list(title = xaxis_title),
               yaxis = list(title =yaxis_title  ,  
                            tickformat=',d'),
               margin = list(m),
               title=paste0("Surveys for ", company_name))      %>% 
        ## remove the download window
        config(displayModeBar = F)
    }

    
  }else if(max(company_df$sum_surveys,na.rm = T) <=2){
    t_youths <- max(company_df$total_placed)
    ## empty plot for adding to before data load
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    text = paste(company_name, "\n",
                 "       Implementing partner\n",
                 "       has less than 3", survey ,"surveys done by \n",
                 t_youths , " placed youth(s)")
    p <-  plot_empty <- ggplot() + 
      annotate("text", x = 4, y = 25, size=8, label = text) + 
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
    p <- ggplotly(plot_empty) %>% 
      layout(xaxis = ax, yaxis = ax)
    
  }
  return(p)
}


## select company from drop down
output$select_co <- renderUI({
all_companies <-as.vector(c("Overall",
    unique(overall_weeks_company$`Company Name`)))

selectInput("company_week", "Select company",
            choices = all_companies,
            selected = "Overall")
})

output$select_co_month <- renderUI({
  all_companies <-as.vector(c("Overall",
                              unique(overall_month_company$`Company Name`)))
  
  selectInput("company_month", "Select company",
              choices = all_companies,
              selected = "Overall")
})


## select the type of placement by company
## select from drop down
output$select_placement <- renderUI({
  type_placement <-as.vector(c("Overall","Host","Internal"))
  
  radioButtons(inputId ="placement", label= "Select placement",
               choiceNames  = type_placement,
               choiceValues =c("overall","host","internal"),
              selected = "Overall",
              inline = T)
})

## weekly graph
company_weekly <- reactive({
  specific_company <- overall_weeks_company %>% 
    filter(`Company Name`==input$company_week)
  
  p <- plot_survey_bars(company_df = specific_company,
                        company_name =input$company_week , survey = "Weekly" )
  
  
})


## plot the bar graph for the weekly surveys 
output$weekly_survey_co <- renderPlotly({
  if(input$company_week=="Overall"){
    plot_ly(overall_weeks, x = ~year_month_week ,
            y = ~weekly_surveys, type = 'bar', name = 'Weekly Surveys',
            marker = list(color = '#708fb2')) %>%
      layout(xaxis = list(title = "Months", tickangle = -45),
             yaxis = list(title = "Weekly Surveys Done"),
             margin = list(m),
             title="Weekly surveys overall",
             barmode = 'group')
  }else{
   company_weekly()
  }
})


## monthly graph
company_monthly <- reactive({
  specific_company <- overall_month_company %>% 
    filter(`Company Name`==input$company_month)
  
  p <- plot_survey_bars(company_df = specific_company,
                        company_name =input$company_month ,
                        survey = "Monthly" )

})


## plot the bar graph for the weekly surveys 
output$monthly_survey_co <- renderPlotly({
  if(input$company_month=="Overall"){
    plot_ly(overall_months, x = ~year_month ,
            y = ~monthly_surveys, type = 'bar', name = 'Monthly Surveys',
            marker = list(color = '#708fb2')) %>%
      layout(xaxis = list(title = "Months", tickangle = -45),
             yaxis = list(title = "Monthly Surveys Done"),
             margin = list(m),
             title="Monthly surveys overall",
             barmode = 'group')
  }else{
    company_monthly()
  }
})


## include table for the sumamries to dowonload
#--- weekly tables
df_weekly_co <- reactive({
  youth_weekly <- youth_weekly %>% 
    filter(Company_Name==input$company_week)
})


# table of te first file
output$weekly_table <-  DT::renderDT({

  
  if(input$company_week=="Overall"){
    df <- youth_weekly %>% 
      select(Company_Name , YouthIDNumber, Placement, weekly_survey_done, Start_date) %>% 
      arrange(desc(weekly_survey_done)) %>% 
      top_n(10)
                
            
    df <- DT::datatable(df,  selection="multiple", escape=FALSE, 
                        options = list(sDom  = '<"top">lrt<"bottom">ip')) #%>% 
   # formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
  }else{
    df <- df_weekly_co()
    df <- df %>% 
      select(Company_Name , YouthIDNumber, Placement, weekly_survey_done, Start_date) %>% 
      arrange(desc(weekly_survey_done)) %>% 
      top_n(10)
  
    df <- DT::datatable(df ,  selection="multiple", escape=FALSE, 
                        options = list(sDom  = '<"top">lrt<"bottom">ip')) #%>% 
     #formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
  }

})

## download the weekly data 
output$download_weekly <- downloadHandler(
  # if(input$company_week=="Overall"){
  #   filename =function() {     paste('data-', Sys.Date(), '.csv', sep='')
  #   },
  #   content = function(tbl) {
  #     write.csv(data, tbl)
  #   }
  # }else{
    filename =function() {     paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(df_weekly_co, file)
    }
  #}
  )


## download weekly data
df_monthly_co <- reactive({
  youth_monthly <- youth_monthly %>% 
    filter(Company_Name==input$company_month)
})


# table of te first file
output$monthly_table <-  DT::renderDT({
  
  
  if(input$company_month=="Overall"){
    df <- youth_monthly %>% 
      select(Company_Name , YouthIDNumber, Placement, monthly_survey_done, Start_date) %>% 
      arrange(desc(monthly_survey_done)) %>% 
      top_n(10)
    
    
    df <- DT::datatable(df,  selection="multiple", escape=FALSE, 
                        options = list(sDom  = '<"top">lrt<"bottom">ip')) #%>% 
    # formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
  }else{
    df <- df_monthly_co()
    df <- df %>% 
      select(Company_Name , YouthIDNumber, Placement, monthly_survey_done, Start_date) %>% 
      arrange(desc(monthly_survey_done)) %>% 
      top_n(10)
    
    df <- DT::datatable(df ,  selection="multiple", escape=FALSE, 
                        options = list(sDom  = '<"top">lrt<"bottom">ip')) #%>% 
    #formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
  }
  
})

## download the province data 
output$download_monthly <- downloadHandler(
  # if(input$company_week=="Overall"){
  #   filename =function() {     paste('data-', Sys.Date(), '.csv', sep='')
  #   },
  #   content = function(tbl) {
  #     write.csv(data, tbl)
  #   }
  # }else{
  filename =function() {     paste('data-', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    write.csv(df_monthly_co, file)
  }
  #}
)

## ---------------------------------------
## youths with weekly monthly or baseline

output$v_bw <- shinydashboard::renderValueBox({
  N_bw <- nrow(baseline_weekly)
  shinydashboard::valueBox(
    "Youths with baseline and weekly ",
    value= N_bw,
    icon = icon("pencil")
  )
})


output$v_bm <- shinydashboard::renderValueBox({
  N_bm <- nrow(baseline_monthly)
  shinydashboard::valueBox(
    "Youths with baseline and monthly ",
    value=N_bm,
    icon = icon("pencil") 
  )
})

output$v_bmw <- shinydashboard::renderValueBox({
  N_bmw <- nrow(baseline_monthly_weekly)
  shinydashboard::valueBox(
    "Youths with baseline,weekly and monthly  ",
    value=N_bmw,
    icon = icon("pencil"),
    color = ifelse(N_bmw > 10, "blue", "red")
  )
})

##---------------------------------------------------------
## Physchometrics
##---------------------------------------------------------

##
plot_pyschometrics <- function(plot_df ,ann_text , labels_plot){
  p <- ggplot(plot_df, aes(value)) +
    geom_histogram(aes(y=..density..),bins = 100 ,colour = "#00FFFF", 
                   fill = "#00FFFF") +
    stat_density(geom="line",color="red", position = 'identity') + 
    facet_wrap(variable ~ .,  labeller = labeller(variable =labels_plot ),
                scales = "free") +
    theme_minimal()  +
    geom_segment(aes(x=min(value, na.rm = T), xend = max(value, na.rm = T) , y=0, yend = 0), size=.3,
                 arrow = arrow(length = unit(0.2,"cm"))) +
    geom_text(data=ann_text,aes(x=x,y=y,label=label,size=0.1),show.legend = F)
  
return(p)
}

## ridges function

pyschometrics_ridges <- function(plot_df ,ann_text , labels_plot , likert_label){

  
  p <- ggplot(plot_df, aes(value , variable, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,
                                 gradient_lwd = 1.) +
    scale_x_continuous(expand = c(0.01, 0)) +
    scale_y_discrete(expand = c(0.01, 0) , labels=labels_plot) +
    scale_fill_viridis(name = "Scale", option = "A" , 
                       label=likert_label
                       ) +
    theme_ridges(font_size = 13, grid = TRUE) + 
    theme(axis.title = element_blank(),
          axis.text.x = element_blank()) 
  
  return(p)
}


## Baseline
##---------------------------------------------------------

## big 5
output$baseline_psy_big_5 <- renderPlot({ #renderPlotly
  ## Big five
  big_5 <- c("sc_agree" , "sc_consc","sc_extra","sc_open" , "sc_stability")
  
  baseline_big_5 <- baseline_psy %>% 
    select(big_5) %>% 
    gather(variable,value ) %>% 
    mutate(var_name_lab=gsub("sc_","",variable)) %>% 
    left_join(labels %>% select(-id_master))
  
  labels_plot <- unique(baseline_big_5$var_label)
  names(labels_plot) <- big_5
  
  
  ann_text <- data.frame(x=c(1.5,4.9),y=c(-.5,-.5),label=c("Low","High"))
  
  likert_label <- c("Strongly disagree","Disagree",
                    "Neither agree nor disagree","Agree","Strongly agree")
  
  pyschometrics_ridges(plot_df=baseline_big_5 ,ann_text=ann_text ,
                       labels_plot=labels_plot , likert_label=likert_label )
  
  ## uncomment for histograms
## plot_pyschometrics(baseline_big_5 ,ann_text, labels_plot)
 
  #ggplotly(p)
  
})



#Group 2:Self-efficacy,Growth Mindset,Locus of control,Resilience/Gri,MMCS,Self-esteem
output$group2_base <- renderPlot({ #renderPlotly
  ## baseline_self_efficacy
  group2_base <- c("sc_eff" ,"sc_eff_job","sc_locus" ,"sc_grit","sc_mmcs_context","sc_mmcs_effort" ,"sc_esteem")
  
  baseline_group2 <- baseline_psy %>% 
    select(group2_base) %>% 
    gather(variable,value ) %>% 
    mutate(var_name_lab=gsub("sc_","",variable)) %>% 
    left_join(labels %>% select(-id_master))
  
  labels_plot <- unique(baseline_group2$var_label)
  names(labels_plot) <- group2_base
  
  
  ann_text <- data.frame(x=c(1.5,4.9),y=c(-.5,-.5),label=c("Low","High"))
  
  likert_label <- c("","Strongly disagree","Disagree",
                    "Neither agree nor disagree","Agree","Strongly agree")
  
  pyschometrics_ridges(plot_df=baseline_group2 ,ann_text=ann_text ,
                       labels_plot=labels_plot , likert_label=likert_label )
  
  ## un comment to display histograms
  ##plot_pyschometrics(baseline_group2 ,ann_text, labels_plot)
  
  
  #ggplotly(p)
  
})


##Group 3
#Present day bias (both scales),Future Orientation
output$group3_base <- renderPlot({ #renderPlotly
  ## baseline_self_efficacy
  group3_base <- c("sc_time","sc_decision" , "sc_detail" ,"sc_control")
  
  baseline_group3 <- baseline_psy %>% 
    select(group3_base) %>% 
    gather(variable,value ) %>% 
    mutate(var_name_lab=gsub("sc_","",variable)) %>% 
    left_join(labels %>% select(-id_master))
  
  labels_plot <- unique(baseline_group3$var_label)
  names(labels_plot) <- group3_base
  
  
  ann_text <- data.frame(x=c(1.5,4.9),y=c(-.5,-.5),label=c("Low","High"))
  likert_label <- c("","Strongly disagree","Disagree",
                    "Neither agree nor disagree","Agree","Strongly agree")
  
  pyschometrics_ridges(plot_df=baseline_group3 ,ann_text=ann_text ,
                       labels_plot=labels_plot , likert_label=likert_label )
  
  ## un comment to display histograms
  #plot_pyschometrics(baseline_group3 ,ann_text, labels_plot)
  

  
})

## Group 4:
#Well-being,Dealing with Stress,Perceived stress scale,Dealing with conflict,Attitudes towards work,WEIP
output$group_4_base <- renderPlot({ #renderPlotly
  ## baseline_self_efficacy
  group_4_base <- c( "sc_well", "sc_dealstress", "sc_percstress",
                     "sc_conflict","sc_attdwork" ,"sc_weip"  )
  
  baseline_group4 <- baseline_psy %>% 
    select(group_4_base) %>% 
    gather(variable,value ) %>% 
    mutate(var_name_lab=gsub("sc_","",variable)) %>% 
    left_join(labels %>% select(-id_master))
  
  labels_plot <- unique(baseline_group4$var_label)
  names(labels_plot) <- group_4_base
  
  
  ann_text <- data.frame(x=c(1.5,4.9),y=c(-.5,-.5),label=c("Low","High"))
  likert_label <- c("","Strongly disagree","Disagree",
                    "Neither agree nor disagree","Agree","Strongly agree")
  
  pyschometrics_ridges(plot_df=baseline_group4 ,ann_text=ann_text ,
                       labels_plot=labels_plot , likert_label=likert_label )
  
  ## un comment to display histograms
  #plot_pyschometrics(baseline_group4 ,ann_text, labels_plot)
  
})




## Monthlty
##---------------------------------------------------------
##Group 1:Dealing with conflict,Well-being,Dealing with Stress,Perceived Stress scale,Self-efficacy
output$group1_month <- renderPlot({ #renderPlotly

  group1_month <- c("sc_conflict" , "sc_well", "sc_dealstress"  , "sc_percstress" ,"sc_eff")
  
  
  monthly_group1 <- monthly_psy %>% 
    select(group1_month) %>% 
    gather(variable,value ) %>% 
    mutate(var_name_lab=gsub("sc_","",variable)) %>% 
    left_join(labels %>% select(-id_master))
  
  labels_plot <- unique(monthly_group1$var_label)
  names(labels_plot) <- group1_month
  
  
  ann_text <- data.frame(x=c(1.5,4.9),y=c(-.5,-.5),label=c("Low","High"))
  likert_label <- c("","Strongly disagree","Disagree",
                    "Neither agree nor disagree","Agree","Strongly agree")
  
  pyschometrics_ridges(plot_df=monthly_group1 ,ann_text=ann_text ,
                       labels_plot=labels_plot , likert_label=likert_label )
  
  ## un comment to display histograms
  #plot_pyschometrics(monthly_group1 ,ann_text, labels_plot)
  
  
  
})



#Group 2:Belonging Motivation 1 Problem solving 
## --Time management Teamwork Motivation 2 Behaviors Employee satisfaction


output$group2_month <- renderPlot({ #renderPlotly
 
  group2_month <- c("sc_belong", "sc_motiv","sc_problem", "sc_team","sc_behave" , "sc_satis")
  
  
  monthly_group2 <- monthly_psy %>% 
    select(group2_month) %>% 
    gather(variable,value ) %>% 
    mutate(var_name_lab=gsub("sc_","",variable)) %>% 
    left_join(labels %>% select(-id_master))
  
  labels_plot <- unique(monthly_group2$var_label)
  names(labels_plot) <- group2_month
  
  
  ann_text <- data.frame(x=c(1.5,4.9),y=c(-.5,-.5),label=c("Low","High"))
  
  likert_label <- c("Disagree","Neither agree nor disagree","Agree","Strongly agree")
  
  pyschometrics_ridges(plot_df=monthly_group2 ,ann_text=ann_text ,
                       labels_plot=labels_plot , likert_label=likert_label )
  
  ## un comment to display histograms
  
 # plot_pyschometrics(monthly_group2 ,ann_text, labels_plot)
  
  
  
})





## Weekly
##---------------------------------------------------------
## weekly pshyco
output$group1_weekly <- renderPlot({ #renderPlotly
  
  weekly_vars <- c("sc_check_in" , "sc_behave" , "sc_satis" )
  
  
  weekly_metrics <- weekly_psy %>% 
    select(weekly_vars) %>% 
    gather(variable,value ) %>% 
    mutate(var_name_lab=gsub("sc_","",variable)) %>% 
    left_join(labels %>% select(-id_master))
  
  labels_plot <- unique(weekly_metrics$var_label)
  names(labels_plot) <- weekly_vars
  
  
  ann_text <- data.frame(x=c(1.5,4.9),y=c(-.5,-.5),label=c("Low","High"))
  
  likert_label <- c("","Strongly disagree","Disagree",
                    "Neither agree nor disagree","Agree","Strongly agree")
  
  pyschometrics_ridges(plot_df=weekly_metrics ,ann_text=ann_text ,
                       labels_plot=labels_plot , likert_label=likert_label )
  
  ## un comment to display histograms
 # plot_pyschometrics(weekly_metrics ,ann_text, labels_plot)
  
  
  
})

##
## Weekly Pyschometrics Longitudinal
##---------------------------------------------------------
##

filter_data <- function(placed_weekly_psy , metric){
  plot_df <-  placed_weekly_psy %>% 
    group_by(user_id) %>% 
    arrange(submitted) %>% 
    mutate(week_rank=1:n()) %>% 
    #mutate(week1_metric=(first(base::get(metric)))) %>% 
    #mutate(change_var=(base::get(metric))/week1_metric) %>% 
    select(yes_week, week_rank,metric, user_id,submitted,`Company Name`,direction_lw )
}

longitudinal_graph_metrics <- function(plot_df , time_rank,metric ){
  
  stat_box_data <- function(y, upper_limit = max(plot_df[metric], na.rm = T) * 1.15) {
    return( 
      data.frame(
        y = 0.95 * upper_limit,
        label = paste('n =', length(y), '\n',
                      'mean =', round(mean(y, na.rm = T), 1), '\n')
      )
    )
  }
  
  plot_df <- plot_df %>% 
    mutate(time_rank_cat=get(time_rank) ,
           time_rank_cat=as.factor(time_rank_cat))
  
 p <- ggplot(plot_df, aes_string("time_rank_cat",metric)) + 
    geom_boxplot(width=0.5) + 
    geom_jitter(aes(color=direction_lw ), alpha=0.9)+
    stat_summary(fun.y=median, geom="line", aes(group=1),  color="black")  + 
    stat_summary(fun.y=median, geom="point", color="black")+
    stat_summary(
      fun.data = stat_box_data, 
      geom = "text", 
      hjust = 0.5,
      vjust = 0.9
    )+
    theme_minimal() +
    scale_color_manual(values = c("#19222B","#F48632", "#939393"),na.translate = F) +
    xlab("Week counter") 
  
  
  # p <- ggplot(plot_df , aes_string(time_rank,metric)) +
  #   geom_line(aes(group=user_id , color=user_id)) + 
  #   geom_smooth(se=FALSE, colour="black", size=2) +
  #   theme_minimal() +
  #   xlab(paste0(time_rank," Count")) +
  #   ylab(paste0("", metric)) +
  #   theme(legend.position = "none")
  
  return(p)
}

## select from drop down
output$select_week_metric <- renderUI({
  weekly_vars <- c("sc_check_in" , "sc_behave" , "sc_satis" )
  selectInput("weekly_metric", "Select the metric to plot",
              choices = weekly_vars,
              selected = "sc_check_in")
})


output$longtudinal_weekly  <- renderPlot({ #renderPlotly
  
  weekly_vars <- c("sc_check_in" , "sc_behave" , "sc_satis" )
  
  plot_df <- filter_data(placed_weekly_psy = placed_weekly_psy  ,  input$weekly_metric ) %>% 
    ungroup()
  
p <- longitudinal_graph_metrics(plot_df,"week_rank",input$weekly_metric  )#input$weekly_metric
 p
})


})