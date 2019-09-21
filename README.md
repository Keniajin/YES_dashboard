# YES Monitoring and Evaluation Framework

This framework shows ME graphs for the YES project. The 


## Data Sources
The data are located under `D:\rjob\Yes_SA\YES_shiny\data` . The `processed` folder contains the cleaned data and the `raw` contains the dataset from the different sources.
This will be updated once we get the database connection. 


## Data Summarised

The data is summarised into three sub-dashboards, which are the **country overview**, **company overview** and the **youth dashboard overview**.
The **Country Overview** sub-dashboard is loaded from the `prov_summary` folder. It has the `ui.R` file which controls the look and feel of the dashboard and the `server.R` which controls the visulaization functions.

The **company overview** sub-dashboard is loaded from the `comp_summary` folder. It has the `ui.R` file which controls the look and feel of the dashboard and the `server.R` which controls the visulaization functions

The **youth dashboard** sub-dashboard is loaded from the `youth_page` folder. It has the `ui.R` file which controls the look and feel of the dashboard and the `server.R` which controls the visulaization functions


## Usage 

The app is run from the `yes_dashboard.RMD` file which call the folder for each of the sub-dashboard. The `styles.CSS` and `billboard.CSS` are the cascading style sheet for the app. The `packages_install.R` file contains a file for installation of the packages.  This will not be needed for if you are not running the file from R. The users will be accesing a web URL which they can use. 