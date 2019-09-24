#########################################################################################################################################################
################## Script for cleaning the weekly data   ##########################################################################
#######################   We utlise the surveyData_weekly.xlsx from Mandla   ###########################################################################################
#####  the cleaned data has is saved as weekly_cleaned.csv #########################################################
rm(list = ls())
## load the package of interest
library(tidyverse)

##weekly <- readxl::read_xlsx("data/raw/from_mandla/surveyData_weekly.xlsx")
weekly <- readr::read_csv(file.path("http://survey.yes4youth.mobi/surveydata/weekly"))
## remove the questions 
weekly_clean <- weekly %>% select(-starts_with("question_text"))


## save the weekly cleaned data 
 
write_csv(weekly_clean, "data/processed/weekly_cleaned.csv")
