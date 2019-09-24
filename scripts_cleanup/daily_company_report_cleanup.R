#########################################################################################################################################################
################## Script for cleaning the merging the daily report data   ##########################################################################
#######################   We utlise the daily_report 04-09-2019.xls file planning to use the URl  ###########################################################################################
#####  the cleaned data are is  as daily_report_cleaned.csv and company registrations #########################################################
rm(list = ls())
## load the package of interest
library(tidyverse)
library(readr)
'%ni%' <- Negate('%in%')
## read in the daily report
daily_read <- readxl::read_xls("data/raw/daily_report 04-09-2019.xls" ,
                               sheet = "Worksheet" )



write_csv(daily_read, "data/processed/daily_report_cleaned.csv")
## company registration
company_status_reg  <- daily_read %>% 
  mutate(on_hold = ifelse(grepl("[Hh]old" ,daily_read$`CRM Status`),"On Hold",
                          ifelse(grepl("[Tt]rack" ,daily_read$`CRM Status`),"On Track",
                                 ifelse(grepl("[Aa]rchive" ,daily_read$`CRM Status`),"Archive",
                                        ifelse(grepl("Potential Host" ,daily_read$`CRM Status`),"Potential Host",
                                               ifelse(grepl("Requested Refund" ,daily_read$`CRM Status`),"Requested Refund",NA))))))
company_status_reg <- company_status_reg %>% 
  mutate(on_hold=ifelse(`Registration Status`=="Registered" & 
                          `CRM Status` %ni% c("Requested Refund","Potential Host", "Archived" ) ,
                        "Registered" ,on_hold))


write_csv(company_status_reg, "data/processed/company_status_reg.csv")