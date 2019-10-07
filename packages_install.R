
## the packages being used in YES dashboard
packages_install <-c("tidyverse","flexdashboard","ggpubr","leaflet",
                 "devtools","sf", "shinyjs", "rgdal","maptools", "rgeo", "readxl",
                 "stringi", "RColorBrewer", "plotly","aweek","ggridges" ,"expss" ,"Hmisc") 


## install packages from bioconductor or CRAN
for(i in 1:length(packagesLoad)){
  if(require(packagesLoad[i], character.only = TRUE)==FALSE){
     install.packages(packagesLoad[i]);
    
    #install bioconductor packages
    print(paste0("missing_", packagesLoad[i]))
    
    if(require(packagesLoad[i], character.only = TRUE)==FALSE) {
      source("http://bioconductor.org/biocLite.R")
      biocLite(packagesLoad[i],suppressUpdates=T,ask=F)
      print(paste0("missing_", packagesLoad[i]))
      
    }
}## close if
  
}

## indpedent packages from shiny
devtools::install_github("hadley/shinySignals")
devtools::install_github("jcheng5/bubbles")
