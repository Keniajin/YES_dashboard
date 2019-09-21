## YES SA data merge code ##
## load the libraries of interest
library(readxl)
library(stringi)
library(plotly)
library(lubridate)
library(tidyverse)


####
## remove the exponetial notation of the data
options(scipen = 999)
## read in the baseline data
baseline <- readxl::read_xlsx("data/raw/from_mandla/surveyData_baseline.xlsx")

## monthly survey
## read in the monthly and weekly data
monthtly <- readxl::read_xlsx("data/raw/from_mandla/surveyData_monthly.xlsx")
weekly <- readxl::read_xlsx("data/raw/from_mandla/surveyData_weekly.xlsx")

## weekly
weekly <- weekly %>% 
  group_by(user_id) %>% 
  arrange(user_id ,submitted) %>% 
  mutate(week_counter=1:n())


##
hist(weekly$week_counter)

# Bar graph of counts
ggplot(data=weekly , aes(x=as.factor(week_counter))) +
  geom_bar(stat="count")

p <- ggplot(data=weekly %>% filter(week_counter<4), 
            aes(x=submitted , y=week_counter , group=as.factor(user_id))) +
  geom_line(aes(color = as.factor(user_id))) +
   guides(colour=FALSE)


plotly::ggplotly(p)  %>% 
  layout(showlegend = FALSE)

## monthly and baseline
base_month <- monthtly  %>% 
  left_join(baseline , by = "user_id")

write_csv(base_month , "data/processed/base_month.csv")

## read in the provided base data

## read in the data that is cleaned by the placed_youth code

prov_money <- placed_youth_unique %>% 
  group_by(province_id, province_new) %>% 
  summarise(no_of_youths=n(), exact_injection=sum(exact_injection) , 
            expected_injection=sum(expected_injection))

prov_earn_cat <- placed_youth_unique %>%ungroup() %>%  
  select(province_id, province_new ,earn_category ,month_salary , `Monthly Salary`) %>%
  group_by(province_id, province_new ,earn_category) %>% 
summarise (n = n()) %>%   mutate(freq =round((n / sum(n))*100 , 2)) %>% 
  select(-n) %>% 
  spread(earn_category,freq)  
  
  
prov_money <- prov_money %>% 
  left_join(prov_earn_cat , by=c("province_id", "province_new"))


write.csv(prov_money , "data/processed/prov_money.csv")



## read in daily report
'%ni%' <- Negate('%in%')
daily_read <- readxl::read_xls("data/raw/daily_report 04-09-2019.xls" ,
                               sheet = "Worksheet" )

total_companies <- daily_read %>% 
 filter(`Registration Status`=="Registered") %>% 
  filter(`CRM Status` %ni% c("Requested Refund","Potential Host", "Archived" , "On Hold - Transport Sector"))


total_companies_n <- length(unique(total_companies$`Organisation Name`))

co_select <- total_companies %>% 
  select(`Registration Status`) %>% 
  group_by(`Registration Status`) %>% 
  summarise(reg_stat=n()) %>% 
  filter(!is.na(`Registration Status`))
co_select %>% filter(`Registration Status`=="Registered") %>% 
  select(reg_stat) %>% unlist()


  plot_ly(data=co_select, labels = ~`Registration Status`, values = ~reg_stat) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Donut charts using Plotly",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



library( "billboarder" ) # for charts
billboarder() %>% 
  bb_donutchart(data = co_select) %>% 
  bb_donut(title = "2010")


billboarder() %>% 
  bb_donutchart(data = data.frame(red = 82, green = 33, blue = 93)) %>% 
  bb_donut(title = "2010")


billboarder() %>% 
  bb_barchart(data = co_select, rotated = TRUE) 


data.frame(red = 82, green = 33, blue = 93) %>% 
  c3(colors = list(red = 'red',
                   green = 'green',
                   blue = 'blue')) %>% 
  c3_donut(title = '#d053ee')
