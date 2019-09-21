#########################################################################################################################################################
################## ## this script cleans the placed youth data   ##########################################################################
#######################   We utlise the Placed Youth by Company_02_09_2019.xlsx from Mandla   ###########################################################################################
#####  the cleaned data has is saved as placed_youth_unique.csv #########################################################
rm(list = ls())

## the aim here is to make sure we have a clean file of the placed youth
library(tidyverse)
library(readxl)
library(lubridate)

## placed youth by company data
placed_youth <- readxl::read_xlsx("data/raw/Placed Youth by Company_02_09_2019.xlsx")
placed_youth <- placed_youth %>% 
  mutate(province_new= tolower(Province)) %>% 
  mutate(province_id=ifelse(province_new=="eastern cape", 1,
                            ifelse(province_new=="free state", 2,
                                   ifelse(province_new=="gauteng", 3,
                                          ifelse(province_new=="kwazulu natal", 4,
                                                 ifelse(province_new=="limpopo", 5,
                                                        ifelse(province_new=="mpumalanga", 6,
                                                               ifelse(province_new=="north west", 7,
                                                                      ifelse(province_new=="northern cape", 8,
                                                                             ifelse(province_new=="western cape", 10,NA))))))))))

## check the youths who have been repeated
## months of placesment
placed_youth <- placed_youth %>% 
  group_by(`Youth IDNumber`) %>% 
  mutate(repeat_counter=1:n(), total_repeats=n()) %>% 
  mutate(total_index=sum(repeat_counter))

## save the youths with a repeat placement
repeat_youths <- placed_youth %>% 
  filter(total_index>1) %>% 
  select(`Youth IDNumber`, Department,`Company Name`, `Start date` ,   repeat_counter , total_repeats ,
         `Work Experience Address`)
dir.create("data/processed/checks")
write_csv(repeat_youths , 'data/processed/checks/repeated_youths.csv')

## save the youths with a date of replacement earlier than project start
youths_with_early_date <- placed_youth %>% 
  filter(`Start date`<as_date("2018-01-07", "ymd")) %>% 
  select(`Youth IDNumber`, Department,`Company Name`, `Start date` ,   repeat_counter , total_repeats ,
         `Work Experience Address`)
write_csv(youths_with_early_date , 'data/processed/checks/youths_with_early_date.csv')


## make a summary of the province money overall
placed_youth_unique <- placed_youth %>% 
  filter(repeat_counter==1) %>%
  mutate(`Start date` = as_date(`Start date`)) %>% 
  ## since we had youths who had an earlier date
  #assumed the early date is at 2018-01-07
  mutate(start_date_new=ifelse(`Start date`<as_date("2018-01-07", "ymd") ,
                               as_date("2018-01-07", "ymd"),`Start date` )) %>% 
  mutate(start_date_new=as_date(start_date_new)) %>% 
  ## generate the work months worked by a youth and then add 1
  mutate(months_worked = (interval(start_date_new , today()) %/% months(1))+1) %>% 
  mutate(month_salary=ifelse(`Monthly Salary`<3500, 3500,`Monthly Salary`),
         less_3500=ifelse(`Monthly Salary`<3500, 1,0),
         earn_category= ifelse(`Monthly Salary`<3500 & `Monthly Salary`>20, "Less than minimum",
                               ifelse(`Monthly Salary`<=20 | is.na(`Monthly Salary`) , "Not Declared",
                                      ifelse(`Monthly Salary`>=3500 & !is.na(`Monthly Salary`), "Minimum met",NA)))) %>% 
  mutate(exact_injection=months_worked*month_salary) %>% 
  mutate(expected_injection=month_salary*12)

## clean up the gender variable
placed_youth_unique <- placed_youth_unique %>% 
  mutate(gender_new=tolower(Gender))

## clean up the phone deliverly status
placed_youth_unique <- placed_youth_unique %>% 
  mutate(phone_deliver=tolower(`Delivery Status`)) %>% 
  mutate(phone_deliver=ifelse(is.na(`IMEI No`),"no phone", phone_deliver))

## generate the year of birth in 
## the first 2 characters in the year of birth
placed_youth_unique <- placed_youth_unique %>% 
  mutate(year=substr(`Youth IDNumber`, start = 1, stop = 2),
         month= substr(`Youth IDNumber`, start = 3, stop = 4) ,
         day=substr(`Youth IDNumber`, start = 5, stop = 6),
         dob=substr(`Youth IDNumber`, start = 1, stop = 6) ,
         dob=ymd(dob))

## function to add prefix for the year
add_year <- function(x, year=1940){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

placed_youth_unique <- placed_youth_unique %>% 
  mutate(dob=add_year(dob))

## calculate the age of the partiipants
placed_youth_unique <- placed_youth_unique %>% 
  mutate(age_in_yrs=as.integer((start_date_new-dob)/365.25))

##save a file with over 35 and under 18
placed_youth_unique %>% 
  filter(age_in_yrs<18 | age_in_yrs>35) %>% 
  select(account_no, `Youth IDNumber`,Province,`Company Name`, `Company Employed`,
         Email , `Start date`,dob ,age_in_yrs) %>% 
write_csv('data/processed/checks/over_age_youths.csv')

## create the age categories
## phone ownership
## placed youth with phones and also own gnowbe ID
placed_youth_unique <- placed_youth_unique %>% 
  mutate(own_phone=ifelse(!is.na(`IMEI No`) & !is.na(`Phone Make and Model`), "YES","NO"))  %>% 
  mutate(own_gnowbe=ifelse(!is.na(`IMEI No`) & !is.na(`Phone Make and Model`) & !is.na(GnowbeID), "YES","NO")) 

## save the cleaned file with unique youths 
write_csv(placed_youth_unique , "data/processed/placed_youth_unique.csv")
