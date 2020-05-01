getCountyMap <- function(switchPop, thematic_date){
  
  ma = getMapData()$ma
  ma.county.data = getMapData()$county
  ma.county.data$Cases.Per.1000 <- round((ma.county.data$Cases/ma.county.data$Population * 1000), 0)
  ma.county.data$County <- as.character(ma.county.data$County)
  colnames(ma)[6] <- "County"
  ma$County <- firstup(ma$County)
  
  
  
  data.apr.1 <- subset(ma.county.data, Date2==thematic_date)
  
  data.joined <- inner_join(ma, data.apr.1, by="County")
  data.joined$Cases <- round(data.joined$Cases, 0)
  
  county.map <- ggplot(data = data.joined, mapping = aes(x = long.x, y = lat.x, group=County)) + 
    #coord_fixed(1.3) + 
    geom_polygon(color = "black", fill = "gray") +
    
    geom_polygon(color= 'black', fill = NA) +
    theme(
      axis.text.x = element_text(colour = "white"),
      axis.text.y = element_text(colour = "white"),
      axis.title.y = element_text(colour = "white"),
      axis.title.x = element_text(colour = "white"),
      axis.title = element_text(colour = "white"),
      plot.background = element_rect(fill = "#282b29"),
      panel.background = element_rect(fill = "#282b29"),
      legend.position="none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )+
    labs(x = 'Longitude', y = 'Latitude')
  
  if(switchPop){
    county.map <- county.map + geom_polygon(data= data.joined ## need data here for just one day
                                            , aes(fill = Cases.Per.1000),  color='black') 
  }else{
    county.map <- county.map + geom_polygon(data= data.joined ## need data here for just one day
                                    , aes(fill = Cases), color="black") 
  }
    county.map <- ggplotly(county.map, tooltip = c("County", "Cases", "Cases.Per.1000"))
    
    return(county.map)
}
  
  