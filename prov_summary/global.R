library(plotly)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(shinydashboard)

## read the province summaries 
prov_money <- read_csv("../data/processed/prov_money.csv")

sa_shp <- rgdal::readOGR("../SA_shp/ZA_province.shp")
sa_shp_data <- broom::tidy(sa_shp, region = "ID_1" )


# centroid <- aggregate(cbind(long,lat) ~ id, data=sa_shp_data, FUN=mean) %>% 
#   left_join(df ,  by=c("id"="id"))
centroid <- sf::st_read("../SA_shp/ZA_province_centroid.shp") %>% 
  mutate(xy=gsub("c","",geometry)) %>% 
  mutate(xy=gsub("\\(|\\)","",xy)) %>% 
  separate(xy, c("long", "lat"), ",") %>% 
  mutate_at(c("lat", "long"),as.double) %>% 
  left_join(prov_money %>% ungroup()   ,  
            by=c("ID_1"="province_id") ) 

## merge shape file with province summary
sa_shp@data <- sa_shp@data %>%  
  left_join(prov_money %>% ungroup() %>% 
              mutate(province_id=as.character(province_id))  ,  
                                          by=c("ID_1"="province_id"))
