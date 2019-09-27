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
placed_youth_unique  <- read_csv("data/processed/placed_youth_unique.csv",guess_max = 1000)

##weekly data
weekly <- read_csv("data/processed/weekly_cleaned.csv")

## monthly data

monthly <- read_csv("data/processed/monthtly_clean.csv")


## placed youth summarries
## placed youth with phones
phone_ownership  <- placed_youth_unique %>% 
  group_by(phone_deliver) %>% 
  summarise(n_own_phone=n())


##   youths with GnowbeID
gnowbe_ownership  <- placed_youth_unique %>% 
  #mutate(own_gnowbe=ifelse(!is.na(`IMEI No`) & !is.na(`Phone Make and Model`) & !is.na(GnowbeID), "YES","NO"))  %>% 
  group_by(own_gnowbe_IMEI) %>% 
  summarise(n_own_gnowbe=n())


##weekly and placed youth merge
## gemerate a numeric youth ID
placed_youth_unique <- placed_youth_unique %>%
  mutate(youth_id_num=`Youth IDNumber`) %>% 
  filter(!is.na(youth_id_num))

## 720 merges
placed_weekly <- weekly %>% 
  inner_join( placed_youth_unique,by=c("user_id"='youth_id_num'))


placed_weekly %>%
    summarise(N=n_distinct(user_id))

## 
placed_weekly <- placed_weekly %>% 
  mutate(submitted=as_date(submitted)) %>% 
  mutate(year_month_week=format(submitted ,"%b%Y") ,
         month=month(submitted ,label = T, abbr = T) ,
         yr_week=paste0("(",month,")","wk",isoweek(submitted),"/",year(submitted)))

##monthly and placed youth merge
overall_weeks <- placed_weekly %>% 
  group_by(year_month_week) %>% 
  summarise(weekly_surveys=n())
overall_weeks  <- overall_weeks %>% 
  mutate(year_month_week=as.factor(zoo::as.yearmon(year_month_week , "%b%Y")))


overall_weeks_company <- placed_weekly %>% 
  group_by(yr_week , `Company Name`) %>% 
  summarise(weekly_surveys=n() ,
            total_youths= n_distinct(user_id))


## 327 merges
placed_monthly <- monthly %>% 
  inner_join( placed_youth_unique,by=c("user_id"='youth_id_num'))

placed_monthly %>%
  summarise(N=n_distinct(user_id))


## baseline merges
placed_baseline <- baseline %>% 
  inner_join( placed_youth_unique,by=c("user_id"='youth_id_num'))

placed_baseline %>%
  summarise(N=n_distinct(user_id))
  