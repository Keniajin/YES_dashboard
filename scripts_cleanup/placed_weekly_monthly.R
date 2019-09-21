#########################################################################################################################################################
################## Script for cleaning the merging the placed youth with the weekly and monthly   ##########################################################################
#######################   We utlise the cleaned files for weekly monthly and palces   ###########################################################################################
#####  the cleaned data are is  as monthly_baseline_cleaned.csv  and weekly_baseline_cleaned.csv#########################################################
rm(list = ls())
## load the package of interest
library(tidyverse)
library(readr)

## read in the baseline youth data 
baseline <- readr::read_csv("data/processed/baseline_cleaned.csv")
## read in the placed youth cleaned data
placed_youth  <- read_csv("data/processed/placed_youth_unique.csv",guess_max = 1000)

##weekly data
weekly <- read_csv("data/processed/weekly_cleaned.csv")

## monthly data

monthly <- read_csv("data/processed/monthtly_clean.csv")


## placed youth summarries
## placed youth with phones
phone_ownership  <- placed_youth %>% 
  mutate(own_phone=ifelse(!is.na(`IMEI No`) & !is.na(`Phone Make and Model`), "YES","NO"))  %>% 
  group_by(own_phone) %>% 
  summarise(n_own_phone=n())


##   youths with GnowbeID
gnowbe_ownership  <- placed_youth %>% 
  mutate(own_gnowbe=ifelse(!is.na(`IMEI No`) & !is.na(`Phone Make and Model`) & !is.na(GnowbeID), "YES","NO"))  %>% 
  group_by(own_gnowbe) %>% 
  summarise(n_own_gnowbe=n())


##weekly and placed youth merge
## gemerate a numeric youth ID
placed_youth <- placed_youth %>%
  mutate(youth_id_num=as.numeric(`Youth IDNumber`)) %>% 
  filter(!is.na(youth_id_num))

## 720 merges
placed_weekly <- weekly %>% 
  inner_join( placed_youth,by=c("user_id"='youth_id_num'))

placed_weekly %>%
    summarise(N=n_distinct(user_id))


##monthly and placed youth merge


## 327 merges
placed_monthly <- monthly %>% 
  inner_join( placed_youth,by=c("user_id"='youth_id_num'))

placed_monthly %>%
  summarise(N=n_distinct(user_id))
  