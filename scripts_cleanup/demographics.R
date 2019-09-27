#########################################################################################################################################################
################## Script for extracting the demographics of the youth and visualizing before loading to dashboard ##########################################################################
#######################   We utlise the survey data, baseline data placed youth cleaned data  ###########################################################################################
#####  the cleaned data has each file for cleaning the data #########################################################
rm(list = ls())
library(readxl)
library(plotly)

## remove the exponetial notation of the data
options(scipen = 999)

## weekly data
## read in the baseline data
## the data is cleaned with the cleaning script
baseline <- readr::read_csv("data/processed/baseline_cleaned.csv")
## read in the placed youth cleaned data
placed_youth_unique <- read_csv("data/processed/placed_youth_unique.csv",guess_max = 1000)

## list the important variables from baseline
base_var <- c("user_id","survey", "submitted",  "measure_0" ,"question_demo_9" , "question_demo_1" ,"educ_level")


## merge baseleine and placed
baseline_placed <- baseline %>% select(base_var) %>% 
  mutate(id_youth=as.numeric(user_id)) %>% 
  inner_join(placed_youth_unique %>% mutate(youth_id=as.numeric(`Youth IDNumber`)), 
             by=c("id_youth"="youth_id"))

## save thed data of placed and baseline data merged
write_csv(baseline_placed , "data/processed/placed_merged_baseline.csv")


## summary statistics
### ----------------gender of the placed youth----------------------------
table(baseline$question_demo_1)
gender_baseline <- baseline %>% 
  select(`question_demo_1`) %>% 
  group_by(`question_demo_1`) %>% 
  summarise(gender=n()) 

plot_ly(data=gender_baseline, labels = ~`question_demo_1`, values = ~gender ,
        textinfo = 'label+percent+value') %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Gender distribution baseline",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T))


gender_placed <- placed_youth_unique %>% ungroup() %>% 
  select(gender_new) %>% 
  group_by(`gender_new`) %>% 
  summarise(gender=n()) 

plot_ly(data=gender_placed, labels = ~`gender_new`, values = ~gender ,
        textinfo = 'label+percent+value') %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Gender distribution of placed youth",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T))

plot_ly(placed_youth_unique %>% filter(!is.na(gender_new)), 
        y = ~age_in_yrs, 
        color = ~gender_new,
        split = ~gender_new , 
        type = "violin" ,
        box = list(visible = T),
        meanline = list(visible = T   ))

### ----------------age gender----------------------------
## 
age_gender <- placed_youth_unique %>% 
  group_by(gender_new, age_in_yrs) %>% 
  summarise(count=n()) %>% 
  spread(gender_new , count)


plot_ly(age_gender, x = ~age_in_yrs, y = ~female, type = 'bar', name = 'Females', 
             marker = list(color = '#708fb2')) %>%
  add_trace(y = ~male, name = 'Males', marker = list(color = '#19222b')) %>%
  layout(xaxis = list(title = "Age in years", tickangle = -45),
         yaxis = list(title = "Count"),
         margin = list(b = 100),
         title="Gender vs age",
         barmode = 'group')


## plot eduction level and gender
educ_gender <- baseline_placed %>% 
  group_by(gender_new, educ_level) %>% 
  summarise(count=n()) %>% 
  spread(gender_new , count)


plot_ly(educ_gender, x = ~educ_level, y = ~female, type = 'bar', name = 'Females', 
        marker = list(color = '#708fb2')) %>%
  add_trace(y = ~male, name = 'Males', marker = list(color = '#19222b')) %>%
  layout(xaxis = list(title = "Age in years", tickangle = -45),
         yaxis = list(title = "Count"),
         margin = list(b = 100),
         title="Education level",
         barmode = 'group')



## weekly surveyrs

plot_ly(overall_weeks, x = ~year_month_week , y = ~weekly_surveys, type = 'bar', name = 'Weekly Surveys', 
        marker = list(color = '#708fb2')) %>%
  layout(xaxis = list(title = "Months", tickangle = -45),
         yaxis = list(title = "Weekly Surveys Done"),
         margin = list(b = 100),
         title="",
         barmode = 'group')

barrows <- overall_weeks_company  %>% 
  filter(`Company Name`=="Nedbank Limited")

a <- list(
  autotick = FALSE,
  ticks = "outside",
  tick0 = 0,
  dtick = 1,
  ticklen = 5,
  tickwidth = 2,
  tickcolor = toRGB("blue")
)
t <- list(
  family = "sans serif",
  size = 5,
  color = toRGB("red"))
plot_ly(barrows, x = ~yr_week , y = ~weekly_surveys, type = 'bar', 
        name = 'Weekly Surveys', text=~paste0(weekly_surveys , " by ", total_youths," youths"), 
        marker = list(color = '#708fb2')) %>%
  #add_text(textfont = t, textposition = "top right") %>%
  layout(xaxis = list(title = "Week of the year", tickangle = -45),
         yaxis = list(title = "Weekly Surveys Done"),
         margin = list(b = 100),
         title="",
         barmode = 'group')
