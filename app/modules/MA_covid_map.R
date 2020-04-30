# ###############
# #  packages   #
# ###############
# 
# # install.packages("shiny")
# # install.packages("leaflet")
# # install.packages("RColorBrewer")
# 
# library(shiny)
# library(leaflet)
# 
# 
# ###############
# #  data prep  #
# ###############
# 
# # read covid data and change the date format
# setwd("D:/REMP/09_Spring_2020/STAT691_project/CORONA/Corona_MA_map")
# data.covid <- read.csv("CountyLevelData.csv")
# data.covid$Date <- format(as.Date(data.covid$Date, "%m/%d/%Y"), "%Y-%m-%d") 
# data.covid$Cases <- ifelse(is.na(data.covid$Cases), 0, data.covid$Cases)
# 
# 
# # read coordinate data
# data.coord <- read.csv("County_Coord.csv")
# 
# # merge data
# data.covid <- merge(data.covid, data.coord, by = "County", all.x = TRUE, all.y = FALSE)
# data.covid <- data.covid[!is.na(data.covid$Longitude), ]
# rm(data.coord)
# 
# 
# 
# ###############
# #  shiny app  #
# ###############
# 
# # UI
# 
# ui <- fluidPage(
#       leafletOutput("map"),
#       absolutePanel(bottom = 10, left = 30,
#                     sliderInput("SlideDate", "Date", min(as.Date(data.covid$Date)), max(as.Date(data.covid$Date)),
#                                 value = max(as.Date(data.covid$Date)), step = 1)
#       )
# )
# 
# 
# # server
# 
# server <- function(input, output, session){
#   
#   output$map <- leaflet::renderLeaflet({
#       leaflet::leaflet() %>%
#       leaflet::addTiles() %>%
#       leaflet::setView(-72.032366, 42.3788774, zoom = 8) %>%   # set the view with the centerpoint
#       leaflet::setMaxBounds(-73, 43, -70.5, 41)  # set maximum bounds of the map
#   })
#   
#   observeEvent(input$SlideDate, {
#     
#       filtered.data <- data.covid[data.covid$Date == input$SlideDate, ]
#       
#       leafletProxy("map") %>%
#       clearMarkers() %>%
#       leaflet::addCircleMarkers(lat = filtered.data$Latitude,
#                                 lng = filtered.data$Longitude,
#                                 radius = sqrt(filtered.data$Cases)/1.5,
#                                 color = 'red', 
#                                 stroke = FALSE, fillOpacity = .5)
#   })
#   
# }
# 
# 
# shinyApp(ui, server)
