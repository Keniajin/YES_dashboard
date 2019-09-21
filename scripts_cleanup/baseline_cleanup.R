#########################################################################################################################################################
################## Script for extracting cleaning the baseline data  ##########################################################################
#######################   We utlise the surveyData_baseline.xlsx from   ###########################################################################################
#####  the cleaned data has is saved as baseline_cleaned.csv #########################################################
rm(list = ls())
## load the package of interest
library(tidyverse)


## read in the baseline data
baseline <- readxl::read_xlsx("data/raw/from_mandla/surveyData_baseline.xlsx")

## cleanup the education 
## create a dataset to view the number of categories we have for education
aa <- baseline %>% 
  group_by(question_demo_1,question_demo_7) %>% 
  summarise(count=n())

## create the clean categories for the education variable
baseline <- baseline %>% 
  mutate(question_demo_7=tolower(question_demo_7))  %>% 
  mutate(educ_level=ifelse(question_demo_7=="matric", "Matric",
                           ifelse(question_demo_7=="trade / technical / vocational training (at a tvet, fet or private college)", "Technical",
                                  ifelse(question_demo_7=="university bachelor‚äôs degree", "Bachelors",
                                         ifelse(question_demo_7=="some high school, no matric certificate","Some high school",
                                                ifelse(question_demo_7=="university honours degree","Honors",
                                                       ifelse(question_demo_7=="master‚äôs degree","Masters",
                                                              ifelse(question_demo_7=="primary school","Primary school",
                                                                     ifelse(question_demo_7=="no schooling completed","No School","Others")))))))))


## remove the questions 
baseline_clean <- baseline %>% select(-starts_with("question_text"))


## save the cleaned data
write_csv(baseline_clean, "data/processed/baseline_cleaned.csv")
