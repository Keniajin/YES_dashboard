library(shiny)
library(shinyFiles)
library(DT) ## making tables
library(shiny)
library(shinySignals)   # devtools::install_github("hadley/shinySignals")
library(dplyr)
library(shinydashboard)
library(bubbles)        # devtools::install_github("jcheng5/bubbles")
library(tidyverse)
library(plotly)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(flexdashboard)
library(shinyjs)
'%ni%' <- Negate('%in%')

#library(billboarder)
placed_youth_unique <- read_csv("data/processed/placed_youth_unique.csv")

daily_read <- readxl::read_xls("data/raw/daily_report 04-09-2019.xls" ,
                               sheet = "Worksheet" )

## read in the daily report 
## confirm with Maria the fitering procedure
total_companies <- daily_read %>% 
  filter(`Registration Status`=="Registered" | `Registration Status`=="Initial Signup" ) %>% 
  filter(`CRM Status` %ni% c("Requested Refund","Potential Host", "Archived" ))
co_select <- total_companies %>% 
  select(`Registration Status`) %>% 
  group_by(`Registration Status`) %>% 
  summarise(reg_stat=n()) %>% 
  filter(!is.na(`Registration Status`))
co_select %>% filter(`Registration Status`=="Registered") %>% 
  select(reg_stat) %>% unlist()


## 
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

## read the province summaries 
prov_money <- read_csv("data/processed/prov_money.csv")

sa_shp <- rgdal::readOGR("SA_shp/ZA_province.shp")
sa_shp_data <- broom::tidy(sa_shp, region = "ID_1" )


# centroid <- aggregate(cbind(long,lat) ~ id, data=sa_shp_data, FUN=mean) %>% 
#   left_join(df ,  by=c("id"="id"))
centroid <- sf::st_read("SA_shp/ZA_province_centroid.shp") %>% 
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

shinyServer(function(input, output, session) {
## hiding the header for the UI
  observeEvent(input$button, {
    js$hidehead('none')           
  })
  observeEvent(input$button2, {
    js$hidehead('')           
  })
  
  
  
# table of te first file
output$tbl <-  DT::renderDT({
  #   folder_choose <- parseDirPath(c(home = wd_this) ,input$folderChoose)
  #   if(input$folderChoose!=""){
  #   all_datas <- list.files(sel_path() , recursive = T)
  #   x <- grep("Flags", readLines(  paste0(folder_choose,"/",all_datas[[1]])  ))-1
  #   d_f <- read.table( paste0(folder_choose,"/",all_datas[[1]]),skip=x, header=TRUE)
  #   df <- DT::datatable(head(d_f,n=20))
  # df
  #   }
    prov_money <- prov_money %>% select(-X1)
    df <- DT::datatable(prov_money)
    df
    
     #options = list(lengthChange = FALSE,
                  #  initComplete = JS('function(setting, json) { alert("done"); }'))
})

## download the province data 
output$downloadData <- downloadHandler(
     filename = function() {
       paste('data-', Sys.Date(), '.csv', sep='')
     },
     content = function(tbl) {
       write.csv(data, tbl)
})



## empty plot for adding to before data load
empty_plot <- reactive({
   text = paste("\n   Plot will appear after.\n",
                "       selecting folder with data\n",
                "       and data processing done")
   plot_empty <- ggplot() + 
     annotate("text", x = 4, y = 25, size=8, label = text) + 
     theme_bw() +
     theme(panel.grid.major=element_blank(),
           panel.grid.minor=element_blank())
   return(plot_empty)
 })

##---------------------------------------------------------
## totals calculation
##---------------------------------------------------------
total_injection_value <- reactive({
  total_injection <-1002734040  
  
})


total_youth_commited <- reactive({
  total_youth_com <-20539 
})


exact_injected <- reactive({
  exact_injected <- sum(prov_money$exact_injection) 
})

total_companies_R <- reactive({
  
  total_companies_n <- length(unique(total_companies$`Organisation Name`))
  
})




##---------------------------------------------------------
##total investment
##---------------------------------------------------------
output$total_injection<-renderInfoBox({
  total_injection <- total_injection_value()
  infoBox("Total Injection: ",paste0("R ",formatC(total_injection,format="d", big.mark = ",") ),
          subtitle = "in the local economies" ,
          icon = shiny::icon("dollar"),
         width = 4,fill = TRUE)
})

### gauge of the exact investment
output$exact_injection <- renderGauge({
  total_injection <- total_injection_value()
  exact_injected <- exact_injected()
  perc_injected <- as.integer((exact_injected/total_injection)*100)
  gauge(perc_injected, min = 0, max = 100, symbol = '%', href = "#details",
        gaugeSectors(success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
        ),label = paste0('Exact : (R',formatC(exact_injected,format="d", big.mark = ","),')') )
})


##---------------------------------------------------------
## total youth commited and placed
##---------------------------------------------------------

output$youth_commited <- renderInfoBox({
  total_youth_com <- formatC( total_youth_commited(),format="d", big.mark = ",") 
  total_companies <- formatC( total_companies_R(),format="d", big.mark = ",") 
  infoBox("Commited Jobs:", total_youth_com,
          subtitle = paste0("by ",total_companies," companies") ,
          icon = shiny::icon("calendar"),
          color = "fuchsia",width = 4,fill = TRUE)
})


output$youth_placed <-renderGauge({
  total_placed_n <- length(unique(placed_youth_unique$`Youth IDNumber`))
  total_youth_com <- total_youth_commited()
  diff_not_placed <- total_youth_com - total_placed_n
  perc_placed <- as.integer((total_placed_n/total_youth_com)*100)
  gauge(perc_placed, 
        min = 0, 
        max = 100, 
        symbol = '%', 
        sectors = gaugeSectors(success = c(90, 100), 
                               warning = c(60, 89),
                               danger = c(0, 59)),
        label = paste0(diff_not_placed , ' Remaining jobs'))
})



##---------------------------------------------------------
## total companies
##---------------------------------------------------------
company_reg_status <- reactive({
  co_select <- total_companies %>% 
    select(`Registration Status`) %>% 
    group_by(`Registration Status`) %>% 
    summarise(reg_stat=n()) %>% 
    filter(!is.na(`Registration Status`))
})

output$total_companies <- renderInfoBox({
  total_companies <- formatC( total_companies_R(),format="d", big.mark = ",") 
  infoBox("Signed up companies: ", total_companies,
         # subtitle = paste0("by n"," companies") ,
          icon = shiny::icon("bar-chart"),
          color = "fuchsia",width = 4,fill = TRUE)
})

output$registered_perc <- renderGauge({
  co_select <- company_reg_status()
  co_interest <- total_companies_R()
  co_reg <- co_select %>% filter(`Registration Status`=="Registered") %>% 
    select(reg_stat) %>% unlist()
  diff_not_reg <- co_interest - co_reg
  perc_reg <- as.integer((co_reg/co_interest)*100)
  gauge(perc_reg, 
        min = 0, 
        max = 100, 
        symbol = '% reg', 
        sectors = gaugeSectors(success = c(90, 100), 
                               warning = c(60, 89),
                               danger = c(0, 59)),
        label = paste0(diff_not_reg , ' not registered'))
})


##---------------------------------------------------------
## youths earning less money
##---------------------------------------------------------


output$less_earn <- renderInfoBox({
not_declared <- 3561
less_than_minimum <- 124
  infoBox("Youths earning",
          HTML(paste("< minimum =",less_than_minimum ,br()),
               paste("Not declared =",not_declared ,br())),
          # subtitle = paste0("by n"," companies") ,
          icon = shiny::icon("euro"),
          color = "fuchsia",width = 4,fill = TRUE)
})


output$earn_gauge <- renderGauge({
  not_declared <- 3561
  gauge(50, 
        min = 0, 
        max = 100, 
        symbol = '% less', 
        sectors = gaugeSectors(success = c(90, 100), 
                               warning = c(60, 89),
                               danger = c(0, 59)),
        label = paste0(not_declared , ' not declared'))
})

##---------------------------------------------------------
## registration pie
##---------------------------------------------------------
# output$bil <- billboarder::renderBillboarder({
#   billboarder() %>% 
#     bb_donutchart(data = data.frame(red = 82, green = 33, blue = 93)) %>% 
#     bb_donut(title = "2010")
# })

output$plot_registered <- renderPlotly({
  co_select <- company_reg_status()
all_co <- total_companies_R()
  plot_ly(data=co_select, labels = ~`Registration Status`, values = ~reg_stat) %>%
    add_pie(hole = 0.6) %>%
    layout(  showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           annotations=list(text=paste(all_co, "Companies"), "showarrow"=F, font=list(size = 20)))
})





output$total_youth <- shinydashboard::renderValueBox({
   total_youth_com <-total_youth_commited()
   #flexdashboard::valueBox(
    #paste0("Total files Batch", total_files),
     #total_files, icon = "fa-comments",
    #color = "olive")
   shinydashboard::valueBox(
     paste0( formatC(total_youth_com,format="d", big.mark = ",")), 
     "Commited Jobs", icon = icon("list"),
     color = "red"
   )
   #shinydashboard::valueBox(total_files, "New Orders", icon = icon("credit-card"))

  #valueBox( "Coss", total_files)
})

##total number of files
output$total_placed <- shinydashboard::renderValueBox({
  total_placed_n <- length(unique(placed_youth_unique$`Youth IDNumber`))
  #flexdashboard::valueBox(
  #paste0("Total files Batch", total_files),
  #total_files, icon = "fa-comments",
  #color = "olive")
  shinydashboard::valueBox(
    paste0(total_placed_n), "Placed youths", icon = icon("list"),
    color = "olive"
  )
  #shinydashboard::valueBox(total_files, "New Orders", icon = icon("credit-card"))
  
  #valueBox( "Coss", total_files)
})





output$total_placed_3<-renderInfoBox({
  total_placed_n <- length(unique(placed_youth_unique$`Youth IDNumber`))
  total_youth_com <- total_youth_commited()
  infoBox("Placed youths ",total_youth_com,
          subtitle = "Subtitle" ,
          icon = shiny::icon("bar-chart"),
          color = "fuchsia",width = 4,fill = TRUE)
})

output$total_placed_4<-renderInfoBox({
  total_placed_n <- length(unique(placed_youth_unique$`Youth IDNumber`))
  total_youth_com <- total_youth_commited()
  infoBox("Placed youths ",total_youth_com,
          subtitle = "Subtitle" ,
          icon = shiny::icon("bar-chart"),
          color = "fuchsia",width = 4,fill = TRUE)
})

output$n_placed_gauge <- renderGauge({
  total_placed_n <- length(unique(placed_youth_unique$`Youth IDNumber`))
  perc_placed <- as.integer((total_placed_n/total_youth_commited())*100)
  gauge(perc_placed, min = 0, max = 100, symbol = '%', 
        gaugeSectors(success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
  ))
})


output$gauge1<-renderGauge({
  gauge(0.5, 
        min = 0, 
        max = 1, 
        sectors = gaugeSectors(success = c(0.5, 1), 
                               warning = c(0.3, 0.5),
                               danger = c(0, 0.3)),label = 'Gauge 1')
})



output$gauge3<-renderGauge({
  gauge(0.7, 
        min = 0, 
        max = 1, 
        sectors = gaugeSectors(success = c(0.5, 1), 
                               warning = c(0.3, 0.5),
                               danger = c(0, 0.3)),label = 'Gauge 3')
})

output$vbox1 <- renderValueBox({ 
  downloadRate=50
  valueBox(
    value = formatC(downloadRate, digits = 1, format = "f"),
    subtitle = "Downloads per sec (last 5 min)",
    icon = icon("area-chart"),
    color = if (downloadRate >= 30) "yellow" else "aqua")
  })



output$map <- renderLeaflet({
  pal <- colorNumeric("Oranges", NULL)
  sa_shp <- sa_shp
  labels <- sprintf(
    "<strong>%s</strong><br/>R %s <sup>(%s youths)</sup><br/><strong>  Youths earning min wage %s &#37;</strong><br/>",
    sa_shp@data$NAME_1, formatC(sa_shp@data$expected_injection,format="d", big.mark = ",") ,
    sa_shp@data$no_of_youths, sa_shp@data$`Minimum met`) %>% lapply(htmltools::HTML)
  
  
  lopt = labelOptions(noHide = TRUE,
                      direction = 'top',
                      textOnly = TRUE)
  
 p<- leaflet(sa_shp) %>%
    addPolygons(color = "red", 
                weight = 1, 
                smoothFactor = 0.5,
                opacity = 1.0, 
                stroke = TRUE,
                fillOpacity = 0.7,
                fillColor = ~pal(exact_injection),
                dashArray = "3",
                #label = ~paste0(NAME_1, ": R ", formatC(total_money,format="d", big.mark = ",")),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto",
                  color="red"),
                highlightOptions = highlightOptions(color = "#666", weight = 5,dashArray = "",
                                                    fillOpacity = 0.7,
                                                    bringToFront = TRUE)) %>% 
    #addLegend(pal = pal, values = ~exact_injection, opacity = 0.7, title = NULL,
             # position = "bottomright") %>% 
    
    addLabelOnlyMarkers( ~long, ~lat, label =  ~NAME_1, data =centroid , 
                         labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) 
  p

})
 
 
  
})