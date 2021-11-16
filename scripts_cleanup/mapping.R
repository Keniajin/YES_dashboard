library(plotly)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
#sa_st <- sf::st_read("SA_shp/ZA_province.shp", quiet = TRUE)
#sa_st <- sa_st %>% mutate(ID_1=as.character(ID_1))

sa_shp <- rgdal::readOGR("SA_shp/ZA_province.shp")
sa_shp_data <- broom::tidy(sa_shp, region = "ID_1" )


prov_money <- read_csv("data/processed/prov_money.csv")
centroid <- aggregate(cbind(long,lat) ~ id, data=sa_shp_data, FUN=mean) %>% 
  dplyr::left_join(df ,  by=c("id"="id"))

centroid <- sf::st_read("SA_shp/ZA_province_centroid.shp") %>% 
  mutate(xy=gsub("c","",geometry)) %>% 
  mutate(xy=gsub("\\(|\\)","",xy)) %>% 
  separate(xy, c("long", "lat"), ",") %>% 
  mutate_at(c("lat", "long"),as.double) %>% 
  left_join(prov_money %>% ungroup()   ,  
            by=c("ID_1"="province_id") ) 


sa_shp_data <- sa_shp_data %>% 
 left_join(prov_money %>% ungroup() %>% 
             mutate(province_id=as.character(province_id))  ,  
           by=c("id"="province_id"))   
  


p <- ggplot(sa_shp_data) +
  geom_polygon(aes(x = long, y = lat, group = group , 
                   fill=expected_injection), color="white") +
  scale_fill_gradientn( colours = brewer.pal( 9 , "Reds" ) ) +
  coord_equal() +
  geom_text(data = centroid, mapping = aes(x=long, y=lat, 
                                           label=paste0(NAME_1 ,"; \n R", 
                                                        formatC(expected_injection ,format="d", big.mark="," )) ))
p
ggplotly(p,  tooltip="text") 

## https://shiny.rstudio.com/articles/permissions.html
###https://www.r-bloggers.com/4-tricks-for-working-with-r-leaflet-and-shiny/
## https://jwyckoff.shinyapps.io/earthquakemap/
## https://rich.shinyapps.io/college_map/
## https://fitzlab.shinyapps.io/cityapp/
## https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example ---interactive map in a way with floating
## https://shiny.rstudio.com/gallery/google-charts.html
sa_shp@data <- sa_shp@data %>%  
  left_join(prov_money %>% ungroup() %>% 
              mutate(province_id=as.character(province_id))  ,  
                                          by=c("ID_1"="province_id"))

pal <- colorNumeric("Oranges", NULL)
labels <- paste0(sa_shp@data$NAME_1, ": R ", 
                 formatC(sa_shp@data$expected_injection,format="d", big.mark = ",")) %>%
  lapply(htmltools::HTML)

labels <- sprintf(
  "<strong>%s</strong><br/>R %s <sup>(%s youths)</sup><br/><strong>  Youths earning min wage %s &#37;</strong><br/>",
  sa_shp@data$NAME_1, formatC(sa_shp@data$expected_injection,format="d", big.mark = ",") ,
  sa_shp@data$no_of_youths, sa_shp@data$`Minimum met`) %>% lapply(htmltools::HTML)

labels_static <- sprintf(
  "<strong>%s</strong> <sup>(%s youths)</sup><br/><br/>",
  sa_shp@data$NAME_1, 
  sa_shp@data$no_of_youths) %>% lapply(htmltools::HTML)


lopt = labelOptions(noHide = TRUE,
                    direction = 'top',
                    textOnly = TRUE)

p <- leaflet(sa_shp) %>%
  addPolygons(color = "black", 
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.7,
              fillColor = ~pal(exact_injection),
              dashArray = "3",
              #label = ~paste0(NAME_1, ": R ", formatC(total_money,format="d", big.mark = ",")),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              highlightOptions = highlightOptions(color = "#666", weight = 5,dashArray = "",
                                                  fillOpacity = 0.7,
                                                  bringToFront = TRUE)) %>% 
  addLegend(pal = pal, values = ~exact_injection, opacity = 0.7, title = NULL,
            position = "bottomright") %>% 
    addLabelOnlyMarkers( ~long, ~lat, label =  labels_static, data =centroid , 
                       labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
p
ggsave("sample_graph.png",p)
