                                        YES Monitoring and Evaluation Framework
_____________________________________________________________________________

This tool automates the M&E Framework for the Youth Employment Service. It provides a quick deep dive into key indicator variables and thus informs corporate action where necessary. The scope is data driven and taps into all data sources pulled from the Corporate Data Warehouse.


## Data Sources
The data are located under `D:\rjob\Yes_SA\YES_shiny\data` . The `processed` folder contains the cleaned data and the `raw` contains the dataset from the different sources. NB: Data storage structure will change and the shiny interface will eventually read data from the data warehouse.This will be updated once we get the database connection. 


## Data Summarised

The data is summarised into three sub-dashboards, which are the **country overview**, **company overview** and the **youth dashboard overview**.

The **Country Overview** sub-dashboard is loaded from the `prov_summary` folder. It has the `ui.R` file which controls the look and feel of the dashboard and the `server.R` which controls the visualisation functions.

The **company overview** sub-dashboard is loaded from the `comp_summary` folder. It has the `ui.R` file which controls the look and feel of the dashboard and the `server.R` which controls the visulaization functions

The **youth dashboard** sub-dashboard is loaded from the `youth_page` folder. It has the `ui.R` file which controls the look and feel of the dashboard and the `server.R` which controls the visualisation functions


## Usage 

The app is ran from the `yes_dashboard.RMD` file which calls the folder for each of the sub-dashboard. The `styles.CSS` and `billboard.CSS` are the cascading style sheet for the app. The `packages_install.R` file contains a file for installation of the packages.  This will not be needed for if you are not running the file from R. The users will be accessing a web URL which they can use. 

## Access Rights
The Youth Employment Service will issue access rights to corporates on the **Company Overview** dashboard with controlled high level data visualisation. Data shown on the **Company Overview** dashboard will reflect indicators of the youth progress on the work readiness experience such as number of Modules completed at a given point in time, number of surveys done as well as supervisor sentiments about the youth. Companies will only view the youth summaries related to the youths placed at the company. There shall be a single login per company to be assigned to a company nominee.

## Run this to update data 

source("scripts/baseline_cleanup.R")
source("scripts/demographics.R")
source("scripts/weekly_cleanup.R")
source("scripts/monthly_cleanup.R")
source("scripts/placed_weekly_monthly.R")
