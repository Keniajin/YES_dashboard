#########################################################################################################################################################
################## Script for extracting cleaning the baseline data  ##########################################################################
#######################   We utlise the surveyData_baseline.xlsx from   ###########################################################################################
#####  the cleaned data has is saved as baseline_cleaned.csv #########################################################
rm(list = ls())
## load the package of interest
library(tidyverse)


## read in the baseline data
#baseline <- readxl::read_xlsx("data/raw/from_mandla/surveyData_baseline.xlsx")

baseline <- readr::read_csv(file.path("http://survey.yes4youth.mobi/surveydata/baseline"))
## cleanup the education 
## create a dataset to view the number of categories we have for education
aa <- baseline %>% 
  group_by(question_demo_1,question_demo_7) %>% 
  summarise(count=n())

## create the clean categories for the education variable
baseline <- baseline %>% 
  mutate(question_demo_7=tolower(question_demo_7))  %>% 
  mutate(educ_level=ifelse(question_demo_7=="matric", 4,
                           ifelse(question_demo_7=="trade / technical / vocational training (at a tvet, fet or private college)", 
                                  5,
                                  ifelse(question_demo_7=="university bachelor’s degree", 6,
                                         ifelse(question_demo_7=="some high school, no matric certificate",3,
                                                ifelse(question_demo_7=="university honours degree",7,
                                                       ifelse(question_demo_7=="master’s degree",8,
                                                              ifelse(question_demo_7=="primary school",2,
                                                                     ifelse(question_demo_7=="no schooling completed",1,9)))))))))

baseline$educ_level <- factor(baseline$educ_level,
                              labels = c("No School" ,"Primary school","Some high school",
                                         "Matric","Technical","Bachelors","Honors","Masters" , "Others"))

## remove the questions 
baseline_clean <- baseline %>% select(-starts_with("question_text"))


## save the cleaned data
write_csv(baseline_clean, "data/processed/baseline_cleaned.csv")
