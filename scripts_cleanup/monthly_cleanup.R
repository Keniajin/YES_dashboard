#########################################################################################################################################################
################## Script for cleaning the monthly data   ##########################################################################
#######################   We utlise the surveyData_monthly.xlsx from Mandla   ###########################################################################################
#####  the cleaned data has is saved as monthly_cleaned.csv #########################################################
rm(list = ls())
## load the package of interest
library(tidyverse)

## monthly survey
## read in the monthly and weekly data
##monthtly <- readxl::read_xlsx("data/raw/from_mandla/surveyData_monthly.xlsx")

## read from URL provided by Xen
#monthtly <-  readr::read_csv(file.path("http://survey.yes4youth.mobi/surveydata/monthly"))

monthtly <-  readr::read_csv(file.path("https://data.yes4youth.co.za/api/monthlySurveyResponsesDetail?csv=1"))

## remove the questions 
monthtly_clean <- monthtly %>% select(-starts_with("question_text"))


## save the weekly cleaned data 
write_csv(monthtly_clean, "data/processed/monthtly_clean.csv")
